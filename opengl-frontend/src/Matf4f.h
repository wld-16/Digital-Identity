//
// Created by wnabo on 01.12.2020.
//

#ifndef DIGITAL_IDENTITY_MATF4F_H
#define DIGITAL_IDENTITY_MATF4F_H

#endif //DIGITAL_IDENTITY_MATF4F_H

#include "json/json.hpp"

namespace wn {
    struct Mat4f {
        float M11;
        float M21;
        float M31;
        float M41;
        float M12;
        float M22;
        float M32;
        float M42;
        float M13;
        float M23;
        float M33;
        float M43;
        float M14;
        float M24;
        float M34;
        float M44;
    };

    void to_json(json &j, const Mat4f &m) {
        j = json{{"M11", m.M11},
                 {"M21", m.M21},
                 {"M31", m.M31},
                 {"M41", m.M41},
                 {"M12", m.M12},
                 {"M22", m.M22},
                 {"M32", m.M32},
                 {"M42", m.M42},
                 {"M13", m.M13},
                 {"M23", m.M23},
                 {"M33", m.M33},
                 {"M43", m.M43},
                 {"M14", m.M14},
                 {"M24", m.M24},
                 {"M34", m.M34},
                 {"M44", m.M44}};
    }

    void from_json(const json &j, Mat4f &m) {
        j.at("M11").get_to(m.M11);
        j.at("M21").get_to(m.M21);
        j.at("M31").get_to(m.M31);
        j.at("M41").get_to(m.M41);
        j.at("M12").get_to(m.M12);
        j.at("M22").get_to(m.M22);
        j.at("M32").get_to(m.M32);
        j.at("M42").get_to(m.M42);
        j.at("M13").get_to(m.M13);
        j.at("M23").get_to(m.M23);
        j.at("M33").get_to(m.M33);
        j.at("M43").get_to(m.M43);
        j.at("M14").get_to(m.M14);
        j.at("M24").get_to(m.M24);
        j.at("M34").get_to(m.M34);
        j.at("M44").get_to(m.M44);
    }
}