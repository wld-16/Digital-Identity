//
// Created by wnabo on 01.12.2020.
//

#ifndef DIGITAL_IDENTITY_QUATERNION_H
#define DIGITAL_IDENTITY_QUATERNION_H

#endif //DIGITAL_IDENTITY_QUATERNION_H

#include "json/json.hpp"

namespace wn {
    struct Quaternion {
        float x;
        float y;
        float z;
        float w;
    };

    void to_json(nlohmann::json &j, const Quaternion &quaternion) {
        j = nlohmann::json{{"x", quaternion.x},{"y", quaternion.y},{"z", quaternion.z},{"w", quaternion.w}};
    }

    void from_json(const nlohmann::json &j, Quaternion &quaternion) {
        j.at("x").get_to(quaternion.x);
        j.at("y").get_to(quaternion.y);
        j.at("z").get_to(quaternion.z);
        j.at("w").get_to(quaternion.w);
    }
}