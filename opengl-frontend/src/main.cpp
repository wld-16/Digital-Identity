

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
#include "Matf4f.h"
#include <boost/interprocess/mapped_region.hpp>
#include "Quaternion.h"
#include "ogl/font_technique.h"

#define WINDOW_WIDTH  1280
#define WINDOW_HEIGHT 1024

static const float FieldDepth = 10.0f;
static const float FieldWidth = 10.0f;

FontTechnique font_technique;


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
        m_persProjInfo.zFar = 1000.0f;

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

        font_technique.Init();
        m_pGameCamera = new Camera(WINDOW_WIDTH, WINDOW_HEIGHT, Pos, Target, Up);
        font_technique.Enable();
        // TODO:
        m_shaderProg
        glUniformMatrix4fv(glGetUniformLocation(shader.ID, "projection"), 1, GL_FALSE, glm::value_ptr(projection));

        font_technique.InitFonts();

        //m_pEffect = new SkinningTechnique();

        //if (!m_pEffect->Init()) {
        //    printf("Error initializing the lighting technique\n");
        //    return false;
        //}

        //m_pEffect->Enable();
        //m_pEffect->SetColorTextureUnit(COLOR_TEXTURE_UNIT_INDEX);
        //m_pEffect->SetDirectionalLight(m_directionalLight);
        //m_pEffect->SetMatSpecularIntensity(0.0f);
        //m_pEffect->SetMatSpecularPower(0);

        const std::string filename = "../res/blender.dae";
        //if (!m_mesh.LoadMesh(filename)) {
        //    printf("Mesh with path \'%s\' load failed\n", filename.c_str());
        //    return false;
        //}
        //Matrix4f identity;
        //identity.InitIdentity();
        //for (size_t i = 0; i < m_mesh.NumBones(); i++) {
        //    lastTransforms.push_back(identity);
        //}

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

    void
    insertJoint(vector<Matrix4f> &boneRotations, nlohmann::json json, std::string jointStr) {

        wn::Quaternion quaternion;
        quaternion = json[jointStr].get<wn::Quaternion>();
        aiQuaternion aiQuaternion(quaternion.w, quaternion.x, quaternion.y, quaternion.z);
        Matrix4f RotationM = Matrix4f(aiQuaternion.GetMatrix());
        boneRotations.push_back(RotationM);
    }

    vector<Matrix4f> insertAllJoints(vector<Matrix4f> &boneRotations, nlohmann::json jointsData) {

        std::array<std::string, 20> jointIdentifiers = {
                "knee-right", "foot-right", "ankle-right", "hip-right", "knee-left",
                "ankle-left", "foot-left", "hip-left", "hip-center", "spine", "shoulder-left",
                "shoulder-center", "head", "elbow-left", "wrist-left", "shoulder-right",
                "elbow-right", "wrist-right", "hand-right", "hand-left"
        };

        std::for_each(jointIdentifiers.begin(), jointIdentifiers.end(),
                      [&, idx = 0](std::string jointStr) mutable {
                          insertJoint(boneRotations, jointsData, jointStr);
                          ++idx;
                      }
        );

        return boneRotations;
    }

    void RenderSceneCB() {
        //m_pGameCamera->OnRender();

        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        font_technique.RenderText("WWWWWWWWWWWW",25.0f, 25.0f,1, Vector3f(1,0,0));

        /*CalcFPS();
        m_pEffect->Enable();


        vector<Matrix4f> Transforms;

        float RunningTime = GetRunningTime();



        if (!is_idle_render) {
            nlohmann::json jointsData;
            bool validKinectdata = kinectDataClient->attemptPopJsonQueue(jointsData);
            if (validKinectdata) {
                Transforms = insertAllJoints(Transforms, jointsData);
            } else {
                Matrix4f identity;
                identity.InitIdentity();
                for (size_t i = 0; i < m_mesh.NumBones(); i++) {
                    Transforms.push_back(this->lastTransforms[i]);
                }
            }

            m_mesh.KinectBoneTransform(Transforms, scale_bigger, translateX);
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
        std::printf("mesh: (%.2f,%.2f,%.2f)\n",m_position. x, m_position.y,m_position.z);

        m_pEffect->SetWVP(p.GetWVPTrans());
        m_pEffect->SetWorldMatrix(p.GetWorldTrans());

        m_mesh.Render();
        RenderFPS();

        Vector4f vector4F(1,1,0,0);
        Vector4f end = p.GetWorldTrans() * vector4F;

        lastTransforms = Transforms;
*/
        glutSwapBuffers();
    }

    void getJsonThread() {
        jsonData = kinectDataClient->getLastJsonFrame();
    }

    void KeyboardCB(OGLDEV_KEY OgldevKey, OGLDEV_KEY_STATE State) {
        switch (OgldevKey) {
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

    Texture *pTexture = NULL;
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
    vector<Matrix4f> lastTransforms;
    DirectionalLight m_directionalLight;
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

    pApp->pTexture = new Texture(GL_TEXTURE_2D, "../res/test.png");

    if (!pApp->pTexture->Load()) {
        return 1;
    }

    //thread kinectWebsocketClient(std::bind(initKinectDataClient, pApp));
    //thread fileReadThread(std::bind(initFileIORead));

    if (!pApp->Init()) {
        return 1;
    }

    pApp->Run();

    delete pApp;

    return 0;
}
