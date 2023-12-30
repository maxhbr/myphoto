// Calculate grayscale value that represents sharpness at a particular image point.
// Used for depth map estimation.

#pragma once
#include "worker.hh"

namespace focusstack {

class Task_FocusMeasure: public ImgTask
{
public:
    Task_FocusMeasure(std::shared_ptr<ImgTask> input, float radius = 0, float threshold = /*200*/0.0f);

private:
    virtual void task();
    std::shared_ptr<ImgTask> m_input;
    float m_radius;
    float m_threshold;
};


}