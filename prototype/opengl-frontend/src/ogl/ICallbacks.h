//
// Created by wnabo on 02.11.2020.
//

#ifndef DIGITAL_IDENTITY_ICALLBACKS_H
#define DIGITAL_IDENTITY_ICALLBACKS_H

#include "ogldev_keys.h"

class ICallbacks
{
public:

    virtual void KeyboardCB(OGLDEV_KEY OgldevKey, OGLDEV_KEY_STATE OgldevKeyState = OGLDEV_KEY_STATE_PRESS) {};

    virtual void PassiveMouseCB(int x, int y) {};

    virtual void RenderSceneCB() {};

    virtual void MouseCB(OGLDEV_MOUSE Button, OGLDEV_KEY_STATE State, int x, int y) {};
};

#endif //DIGITAL_IDENTITY_ICALLBACKS_H
