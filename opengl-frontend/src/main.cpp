

#include <stdio.h>

#include "./ogldev_math_3d.h"
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
#include "Calibration.h"

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
        m_persProjInfo.zNear = 1.0f;
        m_persProjInfo.zFar = 1000.0f;

        m_position = Vector3f(0.0f, 0.0f, 0.0f);

        calibrationUnit.setJointIdentifiers(jointIdentifiers);

    }

    ~App() {
        SAFE_DELETE(m_pEffect);
        SAFE_DELETE(m_pGameCamera);
    }

    bool Init() {

        Vector3f Pos(0.0f, 15.0f, 30.0f);
        Vector3f Target(0.0f, 0.0f, -1.0f);
        Vector3f Up(0.0, 1.0f, 0.0f);

        m_pGameCamera = new Camera(WINDOW_WIDTH, WINDOW_HEIGHT, Pos, Target, Up);

        Pipeline p;
        p.SetCamera(m_pGameCamera->GetPos(), m_pGameCamera->GetTarget(), m_pGameCamera->GetUp());
        p.SetPerspectiveProj(m_persProjInfo);
        p.Scale(0.1f, 0.1f, 0.1f);

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
        Matrix4f identity;
        identity.InitIdentity();
        for (size_t i = 0; i < m_mesh.NumBones(); i++) {
            lastTransforms.push_back(identity);
        }

        calibrationUnit.set_output_t_pose_orientation_offset(m_mesh.getJointOrientationsMap());
        calibrationUnit.calculateOrientationInverse();

        kinect_t_pose_inverse_orientations = calibrationUnit.get_calibration_result();



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

        wn::Quaternion rotation;
        wn::Vector3f position;
        rotation = json[jointStr]["rotation"].get<wn::Quaternion>();
        position = json[jointStr]["position"].get<wn::Vector3f>();
        std::cout << "X: "<< position.x << ", Y:"<< position.y << ", Z: "<< position.z << std::endl;

        Vector3f test = Vector3f(position.x, position.y, position.z);
        joint_positions[jointStr] = test - m_pGameCamera->GetPos();

        aiQuaternion kinectQuat(rotation.w, rotation.x, rotation.y, rotation.z);
        aiQuaternion orientationInverse = kinect_t_pose_inverse_orientations[jointStr];

        aiQuaternion calibratedQuaternion = m_mesh.getJointOrientationsMap()[jointStr] * kinectQuat * orientationInverse;

        Matrix4f RotationM = Matrix4f(calibratedQuaternion.GetMatrix());
        boneRotations.push_back(RotationM);
    }

    vector<Matrix4f> insertAllJoints(vector<Matrix4f> &boneRotations, nlohmann::json jointsData) {

        std::for_each(jointIdentifiers.begin(), jointIdentifiers.end(),
                      [&, idx = 0](std::string jointStr) mutable {
                          insertJoint(boneRotations, jointsData, jointStr);
                          ++idx;
                      }
        );

        return boneRotations;
    }

    double aspect_ratio = 0;

    void reshape(int w, int h) {
        aspect_ratio = (double) w / (double) h;
        glViewport(0, 0, w, h);
    }

    void output(GLfloat x, GLfloat y, char *text) {
        glPushMatrix();
        glTranslatef(x, y, 0);
        glScalef(1 / 152.38 * font_scale, 1 / 152.38 * font_scale, 1 / 152.38 * font_scale);
        for (char *p = text; *p; p++) {
            if (fontType == 0) {
                glutStrokeCharacter(GLUT_STROKE_ROMAN, *p);
            } else if (fontType == 1) {
                glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN, *p);
            } else if (fontType == 2) {
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10, *p);
            } else {
                glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, *p);
            }
        }
        glPopMatrix();
    }

    void RenderText() {
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(-10 * aspect_ratio, 10 * aspect_ratio, -10, 10, -1, 1);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();

        glColor3ub(255, 0, 0);

        std::vector<std::string> displayLines;

        displayLines.push_back("Variables:");
        displayLines.push_back("PosX:" + std::to_string(font_position_x));
        displayLines.push_back("PosY:" + std::to_string(font_position_y));
        displayLines.push_back("Pow:" + std::to_string(power));
        displayLines.push_back("LSp:" + std::to_string(line_spacing));
        displayLines.push_back("Mode:" + std::string(modes[currentMode]));
        displayLines.push_back("Scale:" + std::to_string(font_scale));
        displayLines.push_back("CamX:" + std::to_string(m_pGameCamera->GetPos().x));
        displayLines.push_back("CamY:" + std::to_string(m_pGameCamera->GetPos().y));
        displayLines.push_back("CamZ:" + std::to_string(m_pGameCamera->GetPos().z));

        std::for_each(kinect_t_pose_inverse_orientations.begin(), kinect_t_pose_inverse_orientations.end(), [&](const auto &pair) {
            std::string selected = "  ";
            if (jointIdentifiers[selected_joint_orientation_offset] == pair.first) {
                selected = "->";
            }
            displayLines.push_back(selected + pair.first + ":(" + std::to_string(pair.second.x) + ", " +
                                   std::to_string(pair.second.y) + ", " +
                                   std::to_string(pair.second.z) + "," + std::to_string(pair.second.w) + ")");
        });

        std::for_each(joint_positions.begin(), joint_positions.end(), [&](const auto &pair) {
            displayLines.push_back(
                    pair.first + ":(" + std::to_string(pair.second.x) + ", " + std::to_string(pair.second.y) + ", " +
                    std::to_string(pair.second.z) + ")");
        });

        size_t lineIndex = 0;

        std::for_each(displayLines.begin(), displayLines.end(), [&, lineIndex](const std::string &line) mutable {
            output(font_position_x, font_position_y - line_spacing * lineIndex, const_cast<char *>(line.c_str()));
            lineIndex += 1;
        });;
        m_mesh.NumBones();

    }

    void RenderSkeleton() {
        glColor3f(1, 0, 0);
        glBegin(GL_LINES);
        glLineWidth(10);
        Vector3f jp = joint_positions.at("hand-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("wrist-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("elbow-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("shoulder-right");
        glVertex3f(jp.x, jp.y, jp.z);
        glEnd();
        glFlush();

        glColor3f(0, 1, 0);
        glBegin(GL_LINES);
        glLineWidth(10);
        jp = joint_positions.at("head");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("shoulder-center");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("spine");
        glVertex3f(jp.x, jp.y, jp.z);
        glEnd();
        glFlush();

        glColor3f(1, 0, 0);
        glBegin(GL_LINES);
        glLineWidth(10);
        jp = joint_positions.at("hand-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("wrist-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("elbow-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("shoulder-left");
        glVertex3f(jp.x, jp.y, jp.z);
        glEnd();
        glFlush();

        glColor3f(0, 0, 1);
        glBegin(GL_LINES);
        glLineWidth(10);
        jp = joint_positions.at("foot-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("ankle-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("knee-right");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("hip-right");
        glVertex3f(jp.x, jp.y, jp.z);
        glEnd();
        glFlush();

        glColor3f(0, 0, 1);
        glBegin(GL_LINES);
        glLineWidth(10);
        jp = joint_positions.at("foot-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("ankle-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("knee-left");
        glVertex3f(jp.x, jp.y, jp.z);
        jp = joint_positions.at("hip-left");
        glVertex3f(jp.x, jp.y, jp.z);
        glEnd();
        glFlush();

    }

    void RenderSceneCB() {
        m_pGameCamera->OnRender();
        //glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        CalcFPS();

        m_pEffect->Enable();

        vector<Matrix4f> Transforms;

        float RunningTime = GetRunningTime();

        if (!is_idle_render) {
            nlohmann::json jointsData;
            validKinectdata = kinectDataClient->attemptPopJsonQueue(jointsData);
            if (validKinectdata) {
                Transforms = insertAllJoints(Transforms, jointsData);
                m_mesh.UpdateDebugPositions(Transforms);
            } else {
                Matrix4f identity;
                identity.InitIdentity();
                for (size_t i = 0; i < m_mesh.NumBones(); i++) {
                    Transforms.push_back(this->lastTransforms[i]);
                }
            }
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

        Vector4f vector4F(1, 1, 0, 0);
        Vector4f end = p.GetWorldTrans() * vector4F;


        // TODO: Multiple shaders
        lastTransforms = Transforms;
        m_pEffect->Disable();

        if (joint_positions.size() == 20) {
            RenderSkeleton();
        }
        RenderText();

        glutSwapBuffers();
    }

    void getJsonThread() {
        jsonData = kinectDataClient->getLastJsonFrame();
    }

    void KeyboardCB(OGLDEV_KEY OgldevKey, OGLDEV_KEY_STATE State) {
        switch (OgldevKey) {
            case OGLDEV_KEY_q: {
                kinectDataClient->readJson();
                getJsonThread();
                break;
            }
            case OGLDEV_KEY_i:
                fontType += 1;
                fontType %= 4;
                break;
            case OGLDEV_KEY_o:
                fontType -= 1;
                fontType %= 4;
                break;
            case OGLDEV_KEY_PLUS:
                switch (currentMode) {
                    case 1:
                        font_scale += 1.0f * std::pow(10, power);
                        break;
                    case 2:
                        line_spacing += 1.0f * std::pow(10, power);
                        break;
                }
                break;

            case OGLDEV_KEY_MINUS:
                switch (currentMode) {
                    case 1:
                        font_scale -= 1.0f * std::pow(10, power);
                        break;
                    case 2:
                        line_spacing -= 1.0f * std::pow(10, power);
                        break;
                }
                break;

            // Moving UI
            case OGLDEV_KEY_d:
                font_position_x += 1.0 * std::pow(10, power);
                break;
            case OGLDEV_KEY_a:
                font_position_x -= 1.0 * std::pow(10, power);
                break;
            case OGLDEV_KEY_w:
                font_position_y += 1.0 * std::pow(10, power);
                break;
            case OGLDEV_KEY_s:
                font_position_y -= 1.0 * std::pow(10, power);
                break;

            // Change Power of Text scaling
            case OGLDEV_KEY_PAGE_UP:
                power += 1;
                break;
            case OGLDEV_KEY_PAGE_DOWN:
                power -= 1;
                break;

            // Control Rotation Orientation
            case OGLDEV_KEY_x:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].x -= (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_c:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].y -= (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_v:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].z -= (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_b:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].w -= (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_f:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].x += (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_g:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].y += (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_h:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].z += (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;
            case OGLDEV_KEY_j:
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].w += (1.0 * power);
                kinect_t_pose_inverse_orientations[jointIdentifiers.at(selected_joint_orientation_offset)].Normalize();
                break;

            // Switch Text Settings option
            case OGLDEV_KEY_0:
                currentMode = 0;
                break;
            case OGLDEV_KEY_1:
                currentMode = 1;
                break;
            case OGLDEV_KEY_2:
                currentMode = 2;
                break;
            case OGLDEV_KEY_m:
                selected_joint_orientation_offset++;
                selected_joint_orientation_offset %= 20;
                break;
            case OGLDEV_KEY_n:
                selected_joint_orientation_offset--;
                selected_joint_orientation_offset += 20;
                selected_joint_orientation_offset %= 20;
                break;

            case OGLDEV_KEY_e:
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

    int fontType = 0;
    float font_scale = 0.04f;
    int power = -1;
    float font_position_x = 0.75f;
    float font_position_y = 0.95f;
    float line_spacing = 0.035f;


    PersProjInfo m_persProjInfo;
    OrthoProjInfo m_orthoProjInfo;
    kinect_data_client *kinectDataClient;
    SkinningTechnique *m_pEffect;
    Vector3f m_position;
    SkinnedMesh m_mesh;
    bool is_idle_render = true;
    vector<Matrix4f> lastTransforms;
    DirectionalLight m_directionalLight;
    const char *modes[3] = {"Idle", "Scale", "Line Spacing"};
    int currentMode = 0;
    bool validKinectdata = false;
    std::map<std::string, aiQuaternion> kinect_t_pose_inverse_orientations;
    int selected_joint_orientation_offset = 0;

    std::map<std::string, Vector3f> joint_positions;
    std::array<std::string, 20> jointIdentifiers = {
            "knee-right", "foot-right", "ankle-right", "hip-right", "knee-left",
            "ankle-left", "foot-left", "hip-left", "hip-center", "spine", "shoulder-left",
            "shoulder-center", "head", "elbow-left", "wrist-left", "shoulder-right",
            "elbow-right", "wrist-right", "hand-right", "hand-left"
    };
    Calibration calibrationUnit;

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

    if (!GLUTBackendCreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, false, "Skeleton Demo")) {
        return 1;
    }

    App *pApp = new App();

    //pApp->pTexture = new Texture(GL_TEXTURE_2D, "../res/test.png");

    //if (!pApp->pTexture->Load()) {
    //    return 1;
    //}

    thread kinectWebsocketClient(std::bind(initKinectDataClient, pApp));
    //thread fileReadThread(std::bind(initFileIORead));

    if (!pApp->Init()) {
        return 1;
    }

    pApp->Run();

    delete pApp;

    return 0;
}
