

#include <stdio.h>
#include <string.h>

#include <math.h>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <ogldev_app.h>
#include <iostream>

#include "ogl/glut_backend.h"
#include "ogldev_pipeline.h"
#include "ogldev_camera.h"
#include "ogl/opencv_texture.h"
#include "ogl/lighting_technique.h"
#include "ogl/mesh.h"
#include "ogl/shadow_map_technique.h"
#include "ogl/shadow_map_fbo.h"
#include "ogl/skybox.h"
#include "kinect_data_client.h"

#define WINDOW_WIDTH  1280
#define WINDOW_HEIGHT 1024

static const float FieldDepth = 10.0f;
static const float FieldWidth = 10.0f;

class App : public ICallbacks, public OgldevApp
{
public:

    App()
    {
        m_pLightingEffect = NULL;
        m_pShadowMapEffect = NULL;
        m_pGameCamera = NULL;
        m_pMesh = NULL;
        m_scale = 0.0f;

        m_spotLight.AmbientIntensity = 0.1f;
        m_spotLight.DiffuseIntensity = 0.9f;
        m_spotLight.Color = Vector3f(1.0f, 1.0f, 1.0f);
        m_spotLight.Attenuation.Linear = 0.01f;
        m_spotLight.Position  = Vector3f(-20.0, 20.0, 1.0f);
        m_spotLight.Direction = Vector3f(1.0f, -1.0f, 0.0f);
        m_spotLight.Cutoff =  20.0f;

        m_persProjInfo.FOV = 60.0f;
        m_persProjInfo.Height = WINDOW_HEIGHT;
        m_persProjInfo.Width = WINDOW_WIDTH;
        m_persProjInfo.zNear = 1.0f;
        m_persProjInfo.zFar = 50.0f;
    }

    ~App()
    {
        SAFE_DELETE(m_pLightingEffect);
        SAFE_DELETE(m_pShadowMapEffect);
        SAFE_DELETE(m_pGameCamera);
        SAFE_DELETE(m_pMesh);
    }

    bool Init()
    {
        Vector3f Pos(3.0f, 8.0f, -10.0f);
        Vector3f Target(0.0f, -0.2f, 1.0f);
        Vector3f Up(0.0, 1.0f, 0.0f);

        if (!m_shadowMapFBO.Init(WINDOW_WIDTH, WINDOW_HEIGHT)) {
            return false;
        }

        m_pGameCamera = new Camera(WINDOW_WIDTH, WINDOW_HEIGHT, Pos, Target, Up);

        m_pLightingEffect = new LightingTechnique();

        if (!m_pLightingEffect->Init()) {
            printf("Error initializing the lighting technique\n");
            return false;
        }

        m_pLightingEffect->Enable();
        m_pLightingEffect->SetSpotLights(1, &m_spotLight);
        m_pLightingEffect->SetTextureUnit(0);
        m_pLightingEffect->SetShadowMapTextureUnit(1);

        m_pShadowMapEffect = new ShadowMapTechnique();

        if (!m_pShadowMapEffect->Init()) {
            printf("Error initializing the shadow map technique\n");
            return false;
        }

        m_pSkyBox = new SkyBox(m_pGameCamera, m_persProjInfo);

        if (!m_pSkyBox->Init(".",
                             "../res/sp3right.jpg",
                             "../res/sp3left.jpg",
                             "../res/sp3top.jpg",
                             "../res/sp3bot.jpg",
                             "../res/sp3front.jpg",
                             "../res/sp3back.jpg")) {
            return false;
        }

        m_pMesh = new Mesh();

        return m_pMesh->LoadMesh("../res/curl.obj");
    }

    void Run()
    {
        GLUTBackendRun(this);
    }

    virtual void RenderSceneCB()
    {
        m_pGameCamera->OnRender();
        m_scale += 0.05f;

        ShadowMapPass();
        RenderPass();

        glutSwapBuffers();
    }

    virtual void ShadowMapPass()
    {
        m_shadowMapFBO.BindForWriting();

        glClear(GL_DEPTH_BUFFER_BIT);

        m_pShadowMapEffect->Enable();

        Pipeline p;
        p.Scale(0.1f, 0.1f, 0.1f);
        p.Rotate(0.0f, m_scale, 0.0f);
        p.WorldPos(0.0f, 0.0f, 3.0f);
        p.SetCamera(m_spotLight.Position, m_spotLight.Direction, Vector3f(0.0f, 1.0f, 0.0f));
        p.SetPerspectiveProj(m_persProjInfo);
        m_pShadowMapEffect->SetWVP(p.GetWVPTrans());
        m_pMesh->Render();

        glBindFramebuffer(GL_FRAMEBUFFER, 0);
    }

    virtual void RenderPass()
    {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        m_pLightingEffect->Enable();

        m_pLightingEffect->SetEyeWorldPos(m_pGameCamera->GetPos());

        m_shadowMapFBO.BindForReading(GL_TEXTURE1);

        Pipeline p;
        p.SetPerspectiveProj(m_persProjInfo);

        p.Scale(10.0f, 10.0f, 10.0f);
        p.WorldPos(0.0f, 0.0f, 1.0f);
        p.Rotate(90.0f, 0.0f, 0.0f);
        p.SetCamera(m_pGameCamera->GetPos(), m_pGameCamera->GetTarget(), m_pGameCamera->GetUp());
        m_pLightingEffect->SetWVP(p.GetWVPTrans());
        m_pLightingEffect->SetWorldMatrix(p.GetWorldTrans());
        p.SetCamera(m_spotLight.Position, m_spotLight.Direction, Vector3f(0.0f, 1.0f, 0.0f));
        m_pLightingEffect->SetLightWVP(p.GetWVPTrans());
        m_pSkyBox->Render();

        p.Scale(0.1f, 0.1f, 0.1f);
        p.Rotate(0.0f, m_scale, 0.0f);
        p.WorldPos(0.0f, 0.0f, 3.0f);
        p.SetCamera(m_pGameCamera->GetPos(), m_pGameCamera->GetTarget(), m_pGameCamera->GetUp());
        m_pLightingEffect->SetWVP(p.GetWVPTrans());
        m_pLightingEffect->SetWorldMatrix(p.GetWorldTrans());
        p.SetCamera(m_spotLight.Position, m_spotLight.Direction, Vector3f(0.0f, 1.0f, 0.0f));
        m_pLightingEffect->SetLightWVP(p.GetWVPTrans());
        m_pMesh->Render();
    }

    void KeyboardCB(OGLDEV_KEY OgldevKey, OGLDEV_KEY_STATE State)
    {
        switch (OgldevKey) {
            case OGLDEV_KEY_ESCAPE:
            case OGLDEV_KEY_q:
                GLUTBackendLeaveMainLoop();
                break;
            case OGLDEV_KEY_w:
                kinectDataClient->readJson();
                break;
            default:
                m_pGameCamera->OnKeyboard(OgldevKey);
        }
    }


    virtual void PassiveMouseCB(int x, int y)
    {
        m_pGameCamera->OnMouse(x, y);
    }

    void RequestFrame(){

    }

    void setKinectClient(kinect_data_client* kinectDataClient) {
        this->kinectDataClient = kinectDataClient;
    }

private:

    LightingTechnique* m_pLightingEffect;
    ShadowMapTechnique* m_pShadowMapEffect;
    Camera* m_pGameCamera;
    float m_scale;
    SpotLight m_spotLight;
    Mesh* m_pMesh;
    SkyBox* m_pSkyBox;
    ShadowMapFBO m_shadowMapFBO;
    PersProjInfo m_persProjInfo;
    kinect_data_client* kinectDataClient;
};

kinect_data_client kinectDataClient;

void initKinectDataClient(App* app){
    app->setKinectClient(&kinectDataClient);
    kinectDataClient.run();
}


int main(int argc, char** argv)
{

//    Magick::InitializeMagick(*argv);
    GLUTBackendInit(argc, argv, false, false);

    if (!GLUTBackendCreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, false, "Tutorial 17")) {
        return 1;
    }

    App* pApp = new App();

    //thread t1(std::bind(initKinectDataClient,pApp));

    if (!pApp->Init()) {
        return 1;
    }

    pApp->Run();

    delete pApp;

    return 0;
}
