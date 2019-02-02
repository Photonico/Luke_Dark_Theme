<?php

namespace Admin\Controller;

class EmployeeController extends AdminController {

    /*显示员工信息列表*/
    public function index() {
        $params = I('get.');
        $p = (int)$params['page_num'] > 0 ? $params['page_num'] : 1;
        $num = (int)$params['show_num'] > 0 ? $params['show_num'] : C('LIST_ROWS');

        $data = D('Employee')->getList($params, $num, $p);
        $this->assign('data', $data);

        if (IS_AJAX && IS_GET) {
            $this->display('table');
            exit;
        }

        $this->meta_title = '员工信息列表';
        $this->display();
    }

    /*添加员工信息*/
    public function add() {
        $model = D('Employee');
        $pn = I('post.pn', 1, 'intval');
        if (IS_AJAX && IS_POST) {
            if ($model->addData()) {
                $url = $pn > 1 ? U('index', array('show_num' => C('LIST_ROWS'), 'page_num' => $pn)) : U('index');
                $this->ajaxReturn(array('code' => 1, 'msg' => '保存成功', 'url' => $url));
            } else {
                $this->ajaxReturn(array('code' => 0, 'msg' => $model->getError()));
            }
            exit;
        }
        $this->assign('meta_title', '添加员工信息');
        $this->assign('wp', C('WORK_PLACE'));
        $this->assign('dep', C('DEPARTMENT'));
        $this->assign('job', C('JOB'));
        $this->assign('edu', C('EDUCATION'));
        $this->assign('nat', C('NATION'));
        $this->display();
    }

    /* 编辑员工信息 */
    public function edit() {
        $id = I('get.id', 0, 'intval');
        $pn = I('get.pn', 1, 'intval');
        if ($id <= 0) $this->error('无效参数');
        $fields = 'id,job_num,name,work_now,department,job,section,sex,
                hire_date,id_number,mobile,address,nation,education,status,
                hire_out,remark,social_security_time,social_stop_time,hire_out_type,hire_out_reason,major_remark,cv_remark';
        $info = M('Employee')
            ->field($fields)
            ->where(['id' => $id])
            ->find();
        $info['social_security_time'] = (int)$info['social_security_time'] > 0 ? substr($info['social_security_time'], 0, 10) : '';
        $info['social_stop_time'] = (int)$info['social_stop_time'] > 0 ? substr($info['social_stop_time'], 0, 10) : '';
        $info['hire_out'] = (int)$info['hire_out'] > 0 ? substr($info['hire_out'], 0, 10) : '';
        $info['hire_date'] = substr($info['hire_date'], 0, 10);
        if (!$info) $this->error('无效参数');

        $this->assign('info', $info);
        $this->assign('wp', C('WORK_PLACE'));
        $this->assign('dep', C('DEPARTMENT'));
        $this->assign('job', C('JOB'));
        $this->assign('edu', C('EDUCATION'));
        $this->assign('nat', C('NATION'));
        $this->assign('pn', $pn);
        $this->meta_title = '编辑用户信息 ';
        $this->display();
    }

    /*删除员工信息*/
    public function del() {
        if (IS_AJAX && IS_GET) {
            $model = D('Employee');
            if ($model->delData()) {
                $this->ajaxReturn(array('code' => 1, 'msg' => '删除成功'));
            } else {
                $this->ajaxReturn(array('code' => 0, 'msg' => $model->getError()));
            }
        }
    }

    /* 导出员工信息 */
    public function output() {
        $model = D('Employee');
        if (IS_POST) {
            if ($model->output()) {
                $this->ajaxReturn(array('code' => 1, 'msg' => '保存成功', 'url' => U('index')));
            } else {
                $this->ajaxReturn(array('code' => 0, 'msg' => $model->getError()));
            }
            exit;
        }

        $this->assign('wp', C('WORK_PLACE'));
        $this->assign('dep', C('DEPARTMENT'));
        $this->assign('job', C('JOB'));
        $this->assign('edu', C('EDUCATION'));
        $this->assign('nat', C('NATION'));
        $this->meta_title = '导出用户信息 ';
        $this->display();
    }

    //工作地点变更记录
    public function ajaxGetWp() {
        if (IS_AJAX) {
            $job_num = I('post.job_num');
            if ($job_num) {
                $data = M('Workplace')
                    ->field('job_num,origin,now,create_time')
                    ->where(['job_num' => $job_num])
                    ->order('id asc')
                    ->select();
                if ($data) {
                    $wp = C('WORK_PLACE');
                    foreach ($data as $k => $v) {
                        $data[$k]['origin'] = $wp[(int)$v['origin']];
                        $data[$k]['now'] = $wp[(int)$v['now']];
                        $data[$k]['create_time'] = substr($v['create_time'], 0, 10);
                    }
                }

                $rs['status'] = $data ? 3 : 4;
                $rs['msg'] = '获取成功';
                $rs['data'] = $data;
                $this->ajaxReturn($rs);
            } else {
                $rs['status'] = 2;
                $rs['msg'] = '参数不正确';
                $this->ajaxReturn($rs);
            }
        } else {
            $rs['status'] = 1;
            $rs['msg'] = '请求非法';
            $this->ajaxReturn($rs);
        }
    }

    //部门变更记录
    public function ajaxGetDep() {
        if (IS_AJAX) {
            $job_num = I('post.job_num');
            if ($job_num) {
                $data = M('JobChange')
                    ->field('job_num,origin_depart,origin_job,now_depart,now_job,create_time')
                    ->where(['job_num' => $job_num])
                    ->order('id asc')
                    ->select();
                if ($data) {
                    $dep = C('DEPARTMENT');
                    $job = C('JOB');
                    foreach ($data as $k => $v) {
                        $data[$k]['origin_depart'] = $dep[(int)$v['origin_depart']];
                        $data[$k]['now_depart'] = $dep[(int)$v['now_depart']];
                        $data[$k]['origin_job'] = $job[(int)$v['origin_job']];
                        $data[$k]['now_job'] = $job[(int)$v['now_job']];
                        $data[$k]['create_time'] = substr($v['create_time'], 0, 10);
                    }
                }

                $rs['status'] = $data ? 3 : 4;
                $rs['msg'] = '获取成功';
                $rs['data'] = $data;
                $this->ajaxReturn($rs);
            } else {
                $rs['status'] = 2;
                $rs['msg'] = '参数不正确';
                $this->ajaxReturn($rs);
            }
        } else {
            $rs['status'] = 1;
            $rs['msg'] = '请求非法';
            $this->ajaxReturn($rs);
        }
    }

    //项目组变更记录
    public function ajaxGetSec() {
        if (IS_AJAX) {
            $job_num = I('post.job_num');
            if ($job_num) {
                $data = M('SectionLog')
                    ->field('job_num,origin_section,now_section,create_time')
                    ->where(['job_num' => $job_num])
                    ->order('id asc')
                    ->select();
                if ($data) {
                    $sec = [
                        '1' => '租号玩',
                        '2' => '药都网',
                        '3' => '药都棋牌'
                    ];
                    foreach ($data as $k => $v) {
                        $data[$k]['origin_section'] = $sec[(int)$v['origin_section']];
                        $data[$k]['now_section'] = $sec[(int)$v['now_section']];
                        $data[$k]['create_time'] = substr($v['create_time'], 0, 10);
                    }
                }

                $rs['status'] = $data ? 3 : 4;
                $rs['msg'] = '获取成功';
                $rs['data'] = $data;
                $this->ajaxReturn($rs);
            } else {
                $rs['status'] = 2;
                $rs['msg'] = '参数不正确';
                $this->ajaxReturn($rs);
            }
        } else {
            $rs['status'] = 1;
            $rs['msg'] = '请求非法';
            $this->ajaxReturn($rs);
        }
    }

    //员工搜索数据
    public function ajaxGetInfo() {
        if (IS_AJAX) {
            $st = I('get.st', 0, 'intval');
            if ($st) {
                switch ($st) {
                    case 1://1:工号
                        $data = M('Employee')->getField('job_num', true);
                        $data = array_filter($data);
                        break;
                    case 2://2:姓名
                        $data = M('Employee')->getField('name', true);
                        $data = array_filter($data);
                        break;
                    case 3://3:工作地点
                        $data = C('WORK_PLACE');
                        $data = array_values($data);
                        $data = array_filter($data);
                        break;
                    case 4://4:部门
                        $data = C('DEPARTMENT');
                        $data = array_values($data);
                        $data = array_filter($data);
                        break;
                    case 5://5:联系方式
                        $data = M('Employee')->getField('mobile', true);
                        $data = array_values($data);
                        $data = array_filter($data);
                        $data = array_values($data);
                        break;
                    case 6://6:身份证号
                        $data = M('Employee')->getField('id_number', true);
                        $data = array_values($data);
                        $data = array_filter($data);
                        $data = array_values($data);
                        break;
                    case 7://7:住址
                        $data = M('Employee')->getField('address', true);
                        $data = array_values($data);
                        $data = array_filter($data);
                        $data = array_values($data);
                        break;
                    case 8://8:民族
                        $data = C('NATION');
                        $data = array_values($data);
                        $data = array_filter($data);
                        break;
                }

                $rs['status'] = $data ? 3 : 4;
                $rs['msg'] = '获取成功';
                $rs['data'] = $data;
                $this->ajaxReturn($rs);
            } else {
                $rs['status'] = 2;
                $rs['msg'] = '参数不正确';
                $this->ajaxReturn($rs);
            }
        } else {
            $rs['status'] = 1;
            $rs['msg'] = '请求非法';
            $this->ajaxReturn($rs);
        }
    }

    //按条件搜索数据
    public function ajaxGetEmp() {
        if (IS_AJAX && IS_POST) {
            $data = D('Employee')->getInfo();
            if ($data) {
                $rs['status'] = 3;
                $rs['msg'] = '请求成功';
                $rs['data'] = $data;
                $this->ajaxReturn($rs);
            } else {
                $rs['status'] = 2;
                $rs['msg'] = '请求失败';
                $this->ajaxReturn($rs);
            }
        } else {
            $rs['status'] = 1;
            $rs['msg'] = '请求非法';
            $this->ajaxReturn($rs);
        }
    }

    /*public function input() {
        //文件引入
        require_once THINK_PATH . 'Library/Vendor/PHPexcel/PHPExcel.php';
        require_once THINK_PATH . 'Library/Vendor/PHPexcel/PHPExcel/Writer/Excel2007.php';
        require_once THINK_PATH . 'Library/Vendor/PHPexcel/PHPExcel/IOFactory.php';
        $file = dirname(__FILE__) . '/zhw.xlsx';
        $data = $this->importExecl($file);
        unset($data[1], $data[2], $data[3]);

        $wp = C('WORK_PLACE');
        $dep = C('DEPARTMENT');
        $job = C('JOB');
        $edu = C('EDUCATION');

        foreach ($data as $k => $v) {
            $tmp = [
                'job_num' => $v['C'],
                'name' => $v['D'],
                'work_now' => $v['I'] ? array_search($v['I'], $wp) : 1,
                'department' => $v['E'] ? array_search($v['E'], $dep) : 1,
                'job' => $v['F'] ? array_search($v['F'], $job) : 1,
                'section' => 1,//所属项目组
                'sex' => $v['G'] ? ($v['G'] == '男' ? 1 : 2) : 1,
                'hire_date' => $v['H'],
                'id_number' => $v['J'],
                'mobile' => $v['K'],
                'address' => $v['L'],
                'nation' => $v['M'],
                'education' => $v['N'] ? array_search($v['N'], $edu) : '1',
                'status' => $v['O'] ? ($v['O'] == '在职' ? 1 : 2) : 1,
                'hire_out' => $v['P'],
                'create_time' => date('Y-m-d')
            ];
            M('Employee')->add($tmp);
        }
    }*/

    /**
     *  数据导入
     * @param string $file excel文件
     * @param string $sheet
     * @return string   返回解析数据
     * @throws PHPExcel_Exception
     * @throws PHPExcel_Reader_Exception
     */
    public function importExecl($file = '', $sheet = 0) {
        $file = iconv("utf-8", "gb2312", $file);   //转码
        if (empty($file) OR !file_exists($file)) {
            die('file not exists!');
        }

        $objRead = new \PHPExcel_Reader_Excel2007();   //建立reader对象
        if (!$objRead->canRead($file)) {
            $objRead = new \PHPExcel_Reader_Excel5();
            if (!$objRead->canRead($file)) {
                die('No Excel!');
            }
        }

        $obj = $objRead->load($file);  //建立excel对象
        $currSheet = $obj->getSheet($sheet);   //获取指定的sheet表
        $columnH = $currSheet->getHighestColumn();   //取得最大的列号
        $cellName = $this->columnArray($columnH);
        $columnCnt = array_search($columnH, $cellName);
        $rowCnt = $currSheet->getHighestRow();   //获取总行数

        for ($_row = 1; $_row <= $rowCnt; $_row++) {  //读取内容
            for ($_column = 0; $_column <= $columnCnt; $_column++) {
                $cellId = $cellName[$_column] . $_row;
                //处理日期是数字的问题
                if ($cellName[$_column] == 'H' || $cellName[$_column] == 'P') {
                    $cellValue = $currSheet->getCell($cellId)->getValue() ? gmdate("Y-m-d H:i:s", \PHPExcel_Shared_Date::ExcelToPHP($currSheet->getCell($cellId)->getValue())) : '';
                } else {
                    $cellValue = $currSheet->getCell($cellId)->getValue();
                }
                //$cellValue = $currSheet->getCell($cellId)->getValue();
                $data[$_row][$cellName[$_column]] = $cellValue;
            }
        }

        return $data;
    }

    /**
     * 最大列数组
     * @param $column  最大列
     * User: jingwei
     */
    public function columnArray($column) {
        $columnArr = [];
        if (strlen($column) == 1) {
            $end = ord($column);
            for ($i = 65; $i <= $end; $i++) {
                $columnArr[] = strtoupper(chr($i));
            }

        } elseif (strlen($column) == 2) {
            for ($i = 65; $i <= 90; $i++) {
                $columnArr[] = strtoupper(chr($i));
            }
            $start = ord(substr($column, 0, 1));
            $end = ord(substr($column, 1, 1));
            for ($i = 65; $i <= $start; $i++) {
                for ($j = 65; $j <= 90; $j++) {
                    if ($i == $start && $j == $end) {
                        $columnArr[] = $column;
                        break;
                    } else {
                        $columnArr[] = strtoupper(chr($i)) . strtoupper(chr($j));
                    }
                }
            }
        }

        return $columnArr;
    }

}

