

#include <stdio.h>
#include <string.h>

#include <math.h>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <ogldev_app.h>
#include <iostream>
#include <ogldev_engine_common.h>

#include <shared_mutex>

#include "ogl/glut_backend.h"
#include "ogldev_pipeline.h"
#include "ogldev_camera.h"
#include "ogl/opencv_texture.h"
#include "ogl/mesh.h"
#include "kinect_data_client.h"
#include "ogl/skinning_technique.h"
#include "ogl/opencv_skinned_mesh.h"
#include "json_file_reader.h"
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include "json/json.hpp"

#define WINDOW_WIDTH  1280
#define WINDOW_HEIGHT 1024

static const float FieldDepth = 10.0f;
static const float FieldWidth = 10.0f;


namespace ns {
    struct Mat4f{
        float M11;float M21;float M31;float M41;
        float M12;float M22;float M32;float M42;
        float M13;float M23;float M33;float M43;
        float M14;float M24;float M34;float M44;
    };

    void to_json(json& j, const Mat4f& m) {
        j = json{{"M11", m.M11}, {"M21", m.M21}, {"M31", m.M31}, {"M41", m.M41},
                 {"M12", m.M12}, {"M22", m.M22}, {"M32", m.M32}, {"M42", m.M42},
                 {"M13", m.M13}, {"M23", m.M23}, {"M33", m.M33}, {"M43", m.M43},
                 {"M14", m.M14}, {"M24", m.M24}, {"M34", m.M34}, {"M44", m.M44}};
    }

    void from_json(const json& j, Mat4f& m) {
        j.at("M11").get_to(m.M11); j.at("M21").get_to(m.M21); j.at("M31").get_to(m.M31); j.at("M41").get_to(m.M41);
        j.at("M12").get_to(m.M12); j.at("M22").get_to(m.M22); j.at("M32").get_to(m.M32); j.at("M42").get_to(m.M42);
        j.at("M13").get_to(m.M13); j.at("M23").get_to(m.M23); j.at("M33").get_to(m.M33); j.at("M43").get_to(m.M43);
        j.at("M14").get_to(m.M14); j.at("M24").get_to(m.M24); j.at("M34").get_to(m.M34); j.at("M44").get_to(m.M44);
    }
}

class App : public ICallbacks, public OgldevApp {
public:

    App() {
        m_pGameCamera = NULL;
        m_pEffect = NULL;
        m_directionalLight.Color = Vector3f(1.0f, 1.0f, 1.0f);
        m_directionalLight.AmbientIntensity = 0.55f;
        m_directionalLight.DiffuseIntensity = 0.9f;
        m_directionalLight.Direction = Vector3f(1.0f, 0.0, 0.0);


        m_persProjInfo.FOV = 60.0f;
        m_persProjInfo.Height = WINDOW_HEIGHT;
        m_persProjInfo.Width = WINDOW_WIDTH;
        m_persProjInfo.zNear = 0.10f;
        m_persProjInfo.zFar = 10000.0f;

        m_position = Vector3f(0.0f, 0.0f, 0.0f);
    }

    ~App() {
        SAFE_DELETE(m_pEffect);
        SAFE_DELETE(m_pGameCamera);
    }

    bool Init() {
        Vector3f Pos(0.0f, 10.0f, 20.0f);
        Vector3f Target(0.0f, 0.2f, -1.0f);
        Vector3f Up(0.0, 1.0f, 0.0f);

        m_pGameCamera = new Camera(WINDOW_WIDTH, WINDOW_HEIGHT, Pos, Target, Up);

        m_pEffect = new SkinningTechnique();

        if (!m_pEffect->Init()) {
            printf("Error initializing the lighting technique\n");
            return false;
        }

        m_pEffect->Enable();

        m_pEffect->SetColorTextureUnit(COLOR_TEXTURE_UNIT_INDEX);
        m_pEffect->SetDirectionalLight(m_directionalLight);
        m_pEffect->SetMatSpecularIntensity(0.0f);
        m_pEffect->SetMatSpecularPower(0);

        const std::string filename = "../res/blender.dae";
        if (!m_mesh.LoadMesh(filename)) {
            printf("Mesh with path \'%s\' load failed\n", filename.c_str());
            return false;
        }

#ifndef WIN32
        if (!m_fontRenderer.InitFontRenderer()) {
            return false;
        }
#endif
        return true;
    }

    void Run() {
        GLUTBackendRun(this);
    }

    vector<Matrix4f>
    insertJoint(vector<Matrix4f> &boneRotations, bool validKinect, nlohmann::json json, std::string jointStr,
                ns::Mat4f &mat4f) {

        if (validKinect) {
            mat4f = json[jointStr].get<ns::Mat4f>();

            Matrix4f matrix4F(mat4f.M11,mat4f.M21,mat4f.M31,mat4f.M41,
                              mat4f.M12,mat4f.M22,mat4f.M32,mat4f.M42,
                              mat4f.M13,mat4f.M23,mat4f.M33,mat4f.M43,
                              mat4f.M14,mat4f.M24,mat4f.M34,mat4f.M44);
            std::cout << json[jointStr] << std::endl;

            boneRotations.push_back(matrix4F);
        } else {
            Matrix4f matrix4F;
            matrix4F.InitIdentity();
            boneRotations.push_back(matrix4F);
        }

        return boneRotations;
    }

    vector<Matrix4f> insertAllJoints(vector<Matrix4f> &boneRotations) {
        nlohmann::json jointsData;
        bool validKinectdata = kinectDataClient->attemptPopJsonQueue(jointsData);

        ns::Mat4f mat4;

        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "knee-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "foot-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "ankle-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "hip-right", mat4);

        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "knee-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "ankle-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "foot-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "hip-left", mat4);

        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "hip-center", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "spine", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "shoulder-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "shoulder-center", mat4);

        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "head", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "elbow-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "wrist-left", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "shoulder-right", mat4);

        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "elbow-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "wrist-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "hand-right", mat4);
        boneRotations = insertJoint(boneRotations, validKinectdata, jointsData, "hand-left", mat4);
        return boneRotations;
    }

    virtual void RenderSceneCB() {
        CalcFPS();

        m_pGameCamera->OnRender();

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        m_pEffect->Enable();

        vector<Matrix4f> Transforms;

        float RunningTime = GetRunningTime();

        if (!is_idle_render) {
            Matrix4f identity;
            identity.InitIdentity();
            Transforms = insertAllJoints(Transforms);
            //m_mesh.KinectBoneTransform(Transforms, scale_bigger, translateX);
        } else {
            Matrix4f identity;
            identity.InitIdentity();
            //std::function<float(int)> rotateFactor = [](int i){ return (i+1)*2;};
            for (size_t i = 0; i < m_mesh.NumBones(); i++) {
                //  identity.InitRotateTransform(std::pow(m_mesh.NumBones()/rotateFactor(i),RunningTime/10),std::pow(m_mesh.NumBones()/rotateFactor(i),RunningTime/10),std::pow(m_mesh.NumBones()/rotateFactor(i),RunningTime/10));
                Transforms.push_back(identity);
            }
        }


        for (uint i = 0; i < Transforms.size(); i++) {
            m_pEffect->SetBoneTransform(i, Transforms[i]);
        }

        m_pEffect->SetEyeWorldPos(m_pGameCamera->GetPos());

        Pipeline p;
        p.SetCamera(m_pGameCamera->GetPos(), m_pGameCamera->GetTarget(), m_pGameCamera->GetUp());
        p.SetPerspectiveProj(m_persProjInfo);
        p.Scale(0.1f, 0.1f, 0.1f);

        Vector3f Pos(m_position);
        p.WorldPos(Pos);
        p.Rotate(0.0f, 0.0f, 0.0f);
        m_pEffect->SetWVP(p.GetWVPTrans());
        m_pEffect->SetWorldMatrix(p.GetWorldTrans());

        m_mesh.Render();

        RenderFPS();

        glutSwapBuffers();
    }

    void getJsonThread() {
        jsonData = kinectDataClient->getLastJsonFrame();
    }

    void KeyboardCB(OGLDEV_KEY OgldevKey, OGLDEV_KEY_STATE State) {
        switch (OgldevKey) {
            case OGLDEV_KEY_ESCAPE:
            case OGLDEV_KEY_q:
                std::exit(0);
                break;
            case OGLDEV_KEY_w: {
                kinectDataClient->readJson();
                getJsonThread();
                break;
            }
            case OGLDEV_KEY_i:
                scale_bigger += 1;
                break;
            case OGLDEV_KEY_o:
                scale_bigger -= 1;
                break;
            case OGLDEV_KEY_k:
                translateX += 1;
                break;
            case OGLDEV_KEY_l:
                translateX -= 1;
                break;
            case OGLDEV_KEY_1:
                std::cout << "Play Animation" << std::endl;
                is_idle_render = !is_idle_render;
                //std::shared_ptr<SkinnedMesh> load_mesh = std::make_shared<SkinnedMesh>(m_mesh);
                //thread t1([capture0 = load_mesh]() { return applyAnimation(capture0, "../res/battlecry.dae");});
                break;
            default:
                m_pGameCamera->OnKeyboard(OgldevKey);
        }
    }


    virtual void PassiveMouseCB(int x, int y) {
        m_pGameCamera->OnMouse(x, y);
    }

    void setKinectClient(kinect_data_client *kinectDataClient) {
        this->kinectDataClient = kinectDataClient;
    }

    nlohmann::json jsonData;

private:
    Camera *m_pGameCamera;
    float m_scale;
    float scale_bigger = 1;
    float translateX = 0;
    PersProjInfo m_persProjInfo;
    kinect_data_client *kinectDataClient;
    SkinningTechnique *m_pEffect;
    Vector3f m_position;
    SkinnedMesh m_mesh;
    bool is_idle_render = true;
    DirectionalLight m_directionalLight;
    nlohmann::json renderJson;
};

kinect_data_client kinectDataClient;

void initKinectDataClient(App *app) {
    app->setKinectClient(&kinectDataClient);
    kinectDataClient.run();
}

void initFileIORead() {
    json_file_reader json_file_reader;
    //app->jsonData = json_file_reader.getJsonData();
    json_file_reader.run("../../test.json");
}


int main(int argc, char **argv) {
    GLUTBackendInit(argc, argv, false, false);

    if (!GLUTBackendCreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, false, "Tutorial 17")) {
        return 1;
    }

    App *pApp = new App();

    thread kinectWebsocketClient(std::bind(initKinectDataClient, pApp));
    //thread fileReadThread(std::bind(initFileIORead));

    if (!pApp->Init()) {
        return 1;
    }

    pApp->Run();

    delete pApp;

    return 0;
}
