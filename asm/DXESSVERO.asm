*          DATA SET DXESSVERO  AT LEVEL 056 AS OF 03/09/03                      
*          DATA SET DXESSVERB  AT LEVEL 017 AS OF 09/06/94                      
*PHASE ESSVER                                                                   
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        DXESSVER - LU6.2 ESS TO VERDICT TEST PROGRAM         *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'DXESSVER - LU6.2 TEST PROGRAM'                                  
         MACRO                                                                  
&NAME    PRNT  &A                                                               
&NAME    MVC   P(18),=CL18'&A'                                                  
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         UNPK  WORK(7),DUB(4)                                                   
         MVC   P+20(2),WORK                                                     
         MVI   P+22,C':'                                                        
         MVC   P+23(2),WORK+2                                                   
         MVI   P+25,C':'                                                        
         MVC   P+26(2),WORK+4                                                   
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
         EJECT                                                                  
DXESSVER CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DXESSVER,=A(R13CHAIN),RA,R9                                    
*                                                                               
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=CL8'DXRCVPRT'                             
*                                                                               
         L     RF,=A(APPCLOGR)     A(APPC LOG FILE)                             
         MVC   DCBDDNAM-IHADCB(8,RF),=CL8'DXRCVLOG'                             
*                                                                               
         XC    TESTECB,TESTECB                                                  
         LA    R1,TESTECB                                                       
         STCM  R1,7,APPCPARM+21                                                 
         OI    APPCPARM+20,X'80'                                                
*                                                                               
         MVI   OPERSTOP,C'N'                                                    
         OPEN  (APPCLOGR,OUTPUT)                                                
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                                                               
MOPEN    BAL   RE,APPCOPEN         OPEN APPC LINK                               
         B     MTEST                                                            
*                                                                               
MTEST    BAL   RE,APPCTEST         TEST APPC LINK                               
         BNE   MSHUT                                                            
         B     MPQREQ                                                           
         B     MPQSON                                                           
         B     MPQRCV                                                           
         B     MPQINQ                                                           
         B     MSQL                                                             
         B     MSQE                                                             
*                                                                               
MSQE     BAL   RE,APPCSQE          TEST SQL SUB SYSTEM ERROR MSG                
         BNE   MSHUT                                                            
         B     MCLOS               THIS PRODUCES BAD DUMP IN VERDICT            
*                                                                               
MSQL     BAL   RE,APPCSQL          TEST SQL SUB SYSTEM MESSAGES                 
         BNE   MSHUT                                                            
         B     MCLOS               THIS PRODUCES BAD DUMP IN VERDICT            
*                                                                               
MPQSON   BAL   RE,APPCPQ1          TEST SOON REQUEST                            
         BNE   MSHUT                                                            
         B     MCLOS                                                            
*                                                                               
MPQINQ   BAL   RE,APPCPQIN         TEST PQ REPORT INQUIRY MESSAGE               
         BNE   MSHUT                                                            
         B     MCLOS                                                            
*                                                                               
MPQREQ   BAL   RE,APPCPQRQ         TEST PQ REPORT REQUEST MESSAGE               
         BNE   MSHUT                                                            
         B     MCLOS                                                            
*                                                                               
MPQRCV   BAL   RE,APPCPQRC         TEST PQ REPORT RECEIVED MESSAGE              
         BNE   MSHUT                                                            
         B     MCLOS                                                            
*                                                                               
MTEST2   BAL   RE,APPCTST2         TEST APPC LINK                               
         BNE   MSHUT                                                            
         B     MTEST2                                                           
*                                                                               
MCLOS    BAL   RE,APPCCLOS         APPC CLOSE                                   
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
MSHUT    BAL   RE,SHUTDOWN         SHUT DOWN SUB TASK                           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE APPC LU6.2 LINK TO SERVER LU                             *         
* WAIT FOR OPEN REQUEST FROM REMOTE SERVER LU                         *         
* OPEN IN 'OPEN TO SEND' MODE                                         *         
***********************************************************************         
         SPACE 1                                                                
APPCOPEN NTR1                                                                   
         MVC   APPCACTN,APPCACOA                                                
         MVC   APPCDATA(8),=CL8'DDNYTS3V'                                       
         MVC   APPCDATA+8(8),=CL8'DDNYTS2V'                                     
* ??     MVC   APPCDATA+8(8),=CL8'DMLO900V'                                     
         MVC   APPCDATA+16(8),=CL8'DDSLU62 '                                    
         MVC   APPCDATA+24(8),=CL8'EDICT   '                                    
*                                                                               
         OI    APPCPARM+20,X'80'                                                
         LA    R1,APPCPARM         PARAMETERS TO LU6.2 LINK                     
         LINK  EP=DDSAPPC                                                       
         MVC   P+30(9),=C'REG 15 = '                                            
         EDIT  (RF),(5,P+39),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  OPENEXTRACTSENDER                                                
         B     AOPEOK                                                           
AOPEOK   SR    RC,RC                                                            
AOPENO   LTR   RC,RC                                                            
AOPEX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND TEST RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
APPCTEST NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'C'                                                     
         MVC   ESSHMID,=CL3'001'                                                
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'00'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         LA    RF,ESSHDRX                                                       
         DROP  RF                                                               
         USING ESSXDAT,RF                                                       
         MVC   ESSXTXT,=C'MYNAMEIS'                                             
         MVC   ESSXID,=C'111111'                                                
         MVC   ESSXPSWD,=C'MPETPSW '                                            
         MVC   ESSXNEID,=C'METWRKID'                                            
         MVC   ESSXLUID,=C'LOGICLID'                                            
         MVC   ESSXTPNA,=C'TRNPNAME'                                            
         MVC   ESSXVER,=C'VV'                                                   
         MVC   ESSXLEV,=C'LL'                                                   
         MVC   ESSXDATE,=C'19931028'                                            
         MVC   ESSXTIME,=C'171533'                                              
         MVC   ESSXDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(ESSXDATX-ESSXDAT+ESSHDRX-ESSHDR+2)                       
         MVC   HALF,APPCACSR             SEND FILE                              
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    ATSTSTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    ATSTNEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     ATSTOK                                                           
*                                                                               
ATSTNEXT LA    RE,APPCDTA                                                       
         B     ATSTOK                                                           
*                                                                               
ATSTWAIT BAL   RE,WAITABIT                                                      
         BNE   ATSTNO                                                           
         B     ATSTOK                                                           
*                                                                               
ATSTERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ATESTERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     ATSTNO                                                           
*                                                                               
ATSTSTOP EQU   *                                                                
         PRNT  APPCTESTOPERSTOP                                                 
         B     ATSTOK                                                           
*                                                                               
ATSTOK   SR    RC,RC                                                            
ATSTNO   LTR   RC,RC                                                            
ATSTX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND SQL SERVER SUB SYSTEM ERROR MESSAGE                            *         
***********************************************************************         
         SPACE 1                                                                
APPCSQE  NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'E'                                                     
         MVC   ESSHMID,=CL3'010'                                                
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'12345678'                                           
         MVI   ESSHPRI,C'1'                                                     
         MVC   ESSHSYS,=CL3'MED'                                                
         MVC   ESSHPRG,=CL3'BUY'                                                
         MVC   ESSHUID,=CL8'USERID01'                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING ESQLDAT,RF                                                       
         MVC   ESQLMID,=CL4'0020'                                               
         MVC   ESQLCON,=CL6'123456'                                             
         MVC   ESQLAID,=C'DM'                                                   
         MVI   ESQLSYS,C'M'                                                     
         MVI   ESQLSUB,C'S'                                                     
         MVC   ESQLFNUM,=C'00000001'                                            
         MVC   ESQLDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(ESQLDATX-ESQLDAT+ESSHDR1X-ESSHDR+2)                      
         MVC   HALF,APPCACSR             SEND FILE                              
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    ASQESTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    ASQENEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     ASQEWAIT                                                         
*                                                                               
ASQENEXT LA    RE,APPCDTA                                                       
         B     ASQEWAIT                                                         
*                                                                               
ASQEWAIT BAL   RE,WAITABIT                                                      
         BNE   ASQENO                                                           
         B     ASQEOK                                                           
*                                                                               
ASQEERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ASQETERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     ASQENO                                                           
*                                                                               
ASQESTOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     ASQEOK                                                           
*                                                                               
ASQEOK   SR    RC,RC                                                            
ASQENO   LTR   RC,RC                                                            
ASQEX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND SQL SERVER SUB SYSTEM TEST MESSAGES                            *         
***********************************************************************         
         SPACE 1                                                                
APPCSQL  NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'I'                                                     
         MVC   ESSHMID,=CL3'010'                                                
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'12345678'                                           
         MVI   ESSHPRI,C'1'                                                     
         MVC   ESSHSYS,=CL3'MED'                                                
         MVC   ESSHPRG,=CL3'BUY'                                                
         MVC   ESSHUID,=CL8'USERID01'                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING ESQLDAT,RF                                                       
         MVC   ESQLMID,=AL4(ESQLCOMQ)                                           
* ??     MVC   ESQLMID,=AL4(ESQLRCVQ)                                           
         MVC   ESQLCON,=CL6'000000'                                             
         MVC   ESQLAID,=C'DM'                                                   
         MVI   ESQLSYS,C'C'                                                     
         MVI   ESQLSUB,C'S'                                                     
         MVC   ESQLFNUM,=C'00000001'                                            
         MVC   ESQLDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(ESQLDATX-ESQLDAT+ESSHDR1X-ESSHDR+2)                      
         MVC   HALF,APPCACSR             SEND FILE                              
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    ASQLSTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    ASQLNEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     ASQLWAIT                                                         
*                                                                               
ASQLNEXT LA    RE,APPCDTA                                                       
         B     ASQLWAIT                                                         
*                                                                               
ASQLWAIT BAL   RE,WAITABIT                                                      
         BNE   ASQLNO                                                           
         B     ASQLOK                                                           
*                                                                               
ASQLERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ASQLTERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     ASQLNO                                                           
*                                                                               
ASQLSTOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     ASQLOK                                                           
*                                                                               
ASQLOK   SR    RC,RC                                                            
ASQLNO   LTR   RC,RC                                                            
ASQLX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND PQ REPORT SUB SYSTEM TEST MESSAGES                             *         
***********************************************************************         
         SPACE 1                                                                
APPCPQ1  NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'R'                                                     
         MVC   ESSHMID,=AL3(ESSHPQRQ)                                           
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'22345678'                                           
         MVI   ESSHPRI,C'1'                                                     
         MVC   ESSHSYS,=CL3'CTL'                                                
         MVC   ESSHPRG,=CL3'OL '                                                
         MVC   ESSHUID,=CL8'TCH1    '                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING EPQRDAT,RF                                                       
         MVC   EPQRMID,=AL4(EPQRSONQ)                                           
         MVI   EPQRRCF,EPQRRCFR                                                 
         MVC   EPQRVAL(80),SPACES                                               
         MVC   EPQRVAL(34),=CL34'RFHDR=C4F1D3D6C1C4F1F4000000000000'            
         MVC   EPQRVAL+34(32),=CL32'0A0A0B00000000000000000000000000'           
         MVC   EPQRVAL+66(13),=CL13'0000000000000'                              
         MVC   EPQRVAL+80(80),SPACES                                            
         MVC   EPQRVAL+80(30),=CL30'OLD1 003777 0206OUTPUT 0306REP'             
         MVC   EPQRVAL+110(30),=CL30'ORT 0603JIM 0704SOON*         '            
         MVC   EPQRVAL+160(2),=C'DD'                                            
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(EPQRVAL+160-EPQRDAT+ESSHDR1X-ESSHDR+2)                   
         MVC   HALF,APPCACSR                                                    
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    APQ1STOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    APQ1NEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     APQ1WAIT                                                         
*                                                                               
APQ1NEXT LA    RE,APPCDTA                                                       
         B     APQ1WAIT                                                         
*                                                                               
APQ1WAIT BAL   RE,WAITABIT                                                      
         BNE   APQ1NO                                                           
         B     APQ1OK                                                           
*                                                                               
APQ1ERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'APQ1TERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     APQ1NO                                                           
*                                                                               
APQ1STOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     APQ1OK                                                           
*                                                                               
APQ1OK   SR    RC,RC                                                            
APQ1NO   LTR   RC,RC                                                            
APQ1X    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND PQ REPORT INQUIRY MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
APPCPQIN NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'R'                                                     
         MVC   ESSHMID,=AL3(ESSHPQRQ)                                           
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'12345679'                                           
         MVI   ESSHPRI,C'1'                                                     
         MVC   ESSHSYS,=CL3'CTL'                                                
         MVC   ESSHPRG,=CL3'OL '                                                
         MVC   ESSHUID,=CL8'TCH1    '                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING EPQRDAT,RF                                                       
         MVC   EPQRMID,=AL4(EPQRINQQ)                                           
         MVC   EPQRCON,=CL6'000000'                                             
         MVC   EPQRSID,=CL3'ESS'                                                
         MVC   EPQRNUM,=CL5'00002'                                              
         MVC   EPQRDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(EPQRDATX-EPQRDAT+ESSHDR1X-ESSHDR+2)                      
         MVC   HALF,APPCACSR                                                    
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    APQISTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    APQINEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     APQIWAIT                                                         
*                                                                               
APQINEXT LA    RE,APPCDTA                                                       
         B     APQIWAIT                                                         
*                                                                               
APQIWAIT BAL   RE,WAITABIT                                                      
         BNE   APQINO                                                           
         B     APQIOK                                                           
*                                                                               
APQIERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'APQ1TERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     APQINO                                                           
*                                                                               
APQISTOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     APQIOK                                                           
*                                                                               
APQIOK   SR    RC,RC                                                            
APQINO   LTR   RC,RC                                                            
APQIX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND PQ REPORT REQUEST MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
APPCPQRQ NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'R'                                                     
         MVC   ESSHMID,=AL3(ESSHPQRQ)                                           
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'2FFFFFFF'                                           
         MVI   ESSHPRI,C'7'                                                     
*        MVC   ESSHSYS,=CL3'   '                                                
*        MVC   ESSHPRG,=CL3'   '                                                
         MVC   ESSHUID,=CL8'TCH1    '                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING EPQRDAT,RF                                                       
         MVC   EPQRMID,=AL4(EPQRREQQ)                                           
         MVC   EPQRCON,=CL6'000000'                                             
         MVC   EPQRSID,=CL3'JIM'                                                
         MVC   EPQRNUM,=CL5'00011'                                              
         MVC   EPQRBREP,=CL5'00000'                                             
         LA    RE,EPQRBLCK                                                      
         USING EPQRBENT,RE                                                      
         MVC   EPQRBSID,=CL3'ESS'                                               
         MVC   EPQRBNUM,=CL5'00050'                                             
         LA    RE,EPQRBLEN(RE)                                                  
         MVC   EPQRBSID,=CL3'ESS'                                               
         MVC   EPQRBNUM,=CL5'00001'                                             
         LA    RE,EPQRBLEN(RE)                                                  
         MVC   EPQRBSID,=CL3'ESS'                                               
         MVC   EPQRBNUM,=CL5'00067'                                             
         DROP  RE                                                               
         MVC   EPQRDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(EPQRDATX-EPQRDAT+ESSHDR1X-ESSHDR+2)                      
         MVC   HALF,APPCACSR                                                    
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    APQQSTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    APQQNEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     APQQWAIT                                                         
*                                                                               
APQQNEXT LA    RE,APPCDTA                                                       
         B     APQQWAIT                                                         
*                                                                               
APQQWAIT BAL   RE,WAITABIT                                                      
         BNE   APQQNO                                                           
         B     APQQOK                                                           
*                                                                               
APQQERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'APQ1TERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     APQQNO                                                           
*                                                                               
APQQSTOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     APQQOK                                                           
*                                                                               
APQQOK   SR    RC,RC                                                            
APQQNO   LTR   RC,RC                                                            
APQQX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RECV PQ REPORT REQUEST MESSAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
APPCPQRC NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         USING ESSHDR,RF                                                        
         LA    RF,APPCDTA                                                       
         MVC   ESSHDDS,=C'DD'                                                   
         MVI   ESSHMTY,C'I'                                                     
         MVC   ESSHMID,=AL3(ESSHPQRQ)                                           
         MVC   ESSHSID,=XL2'0064'                                               
         MVC   ESSHDTTM,=XL4'12345678'                                          
         MVI   ESSHPCF,X'80'                                                    
         MVI   ESSHMFF,X'00'                                                    
         MVI   ESSHHTY,X'C0'                                                    
         MVC   ESSHDRX(2),=C'DD'                                                
         MVC   ESSHREF,=XL4'12345679'                                           
*        MVI   ESSHPRI,C' '                                                     
*        MVC   ESSHSYS,=CL3'   '                                                
*        MVC   ESSHPRG,=CL3'   '                                                
         MVC   ESSHUID,=CL8'TCH1    '                                           
         MVC   ESSHPWD,=CL8'PASSWORD'                                           
         MVI   ESSH1FL,X'00'                                                    
         MVC   ESSHDR1X(2),=C'DD'                                               
         LA    RF,ESSHDR1X                                                      
         DROP  RF                                                               
         USING EPQRDAT,RF                                                       
         MVC   EPQRMID,=AL4(EPQRRCVQ)                                           
         MVC   EPQRCON,=CL6'000000'                                             
         MVC   EPQRSID,=CL3'ESS'                                                
         MVC   EPQRNUM,=CL5'00002'                                              
         MVC   EPQRDATX(2),=C'DD'                                               
         DROP  RF                                                               
         SR    R1,R1                                                            
         ICM   R1,3,=Y(EPQRDATX-EPQRDAT+ESSHDR1X-ESSHDR+2)                      
         MVC   HALF,APPCACSR                                                    
         BAL   RE,APPCCOM                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    APRCSTOP                                                         
         PRNT  SENTCOMMAND                                                      
*                                                                               
         CLI   APPCHDR,X'40'                                                    
         BE    APRCNEXT                                                         
         CLI   APPCHDR,X'80'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     APRCWAIT                                                         
*                                                                               
APRCNEXT LA    RE,APPCDTA                                                       
         B     APRCWAIT                                                         
*                                                                               
APRCWAIT BAL   RE,WAITABIT                                                      
         BNE   APRCNO                                                           
         B     APRCOK                                                           
*                                                                               
APRCERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'APQ1TERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     APRCNO                                                           
*                                                                               
APRCSTOP EQU   *                                                                
         PRNT  APPC1TSTOPERSTOP                                                 
         B     APRCOK                                                           
*                                                                               
APRCOK   SR    RC,RC                                                            
APRCNO   LTR   RC,RC                                                            
APRCX    XIT1                                                                   
***********************************************************************         
* SEND TEST RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
APPCTST2 NTR1                                                                   
*                                                                               
         PRNT  ENTERAPPCTST2                                                    
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'SEND FLUSH'                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACSF             SEND FILE                              
         BAL   RE,APPCCOM                                                       
         BNE   ATS2ERR                                                          
         PRNT  SENTCOMMANDTST2                                                  
         B     ATS2OK                                                           
*                                                                               
ATS2ERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ATST2ERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     ATS2NO                                                           
*                                                                               
ATS2STOP EQU   *                                                                
         PRNT  APPCTST2OPERSTOP                                                 
         B     ATS2OK                                                           
*                                                                               
ATS2OK   SR    RC,RC                                                            
ATS2NO   LTR   RC,RC                                                            
ATS2X    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CLOSE APPC LU6.2 LINK TO SERVER LU                                  *         
***********************************************************************         
         SPACE 1                                                                
APPCCLOS NTR1                                                                   
         PRNT  XTCLOSEPREPARE                                                   
         MVC   APPCACTN,APPCACCP   ISSUE CLOSE/PREPARE                          
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         PRNT  XTRSENDERPCLOSE                                                  
         B     ACLOOK                                                           
*                                                                               
ACLO010  DS    0H                                                               
         PRNT  EZCLOSE                                                          
         MVC   APPCACTN,APPCACC    ISSUE CLOSE                                  
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         DS    0H                                                               
         PRNT  XTRSENDERCLOSED                                                  
         B     ACLOOK                                                           
ACLOOK   SR    RC,RC                                                            
ACLONO   LTR   RC,RC                                                            
ACLOX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SHUT DOWN SUB TASK                                                  *         
***********************************************************************         
         SPACE 1                                                                
SHUTDOWN NTR1                                                                   
         PRNT  SHUTDOWNEDICTEXT                                                 
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WAIT A BIT                                                          *         
***********************************************************************         
         SPACE 1                                                                
WAITABIT NTR1                                                                   
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         ST    R2,FULL                                                          
         STIMERM SET,ID=STIMERID,BINTVL=FULL,WAIT=YES                           
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TRANSMIT DATA VIA APPC LU6.2 COMMAND                     *         
*                                                                     *         
* ON ENTRY, R1 = LENGTH OF DATA                                       *         
*           FIELD 'HALF' CONTAINS APPC COMMAND                        *         
***********************************************************************         
         SPACE 1                                                                
APPCCOM  NTR1                                                                   
         LR    RF,R1                                                            
         LA    RF,4(RF)            L'DATA + LENGTH                              
         STCM  RF,3,APPCLEN                                                     
         LA    R2,8(RF)            L'DATA + OVERHEAD                            
         LA    R3,APPCDTA                                                       
         AR    R3,R1                                                            
         MVC   0(2,R3),=X'0D25'    TERMINATE RECORD WITH CRLF                   
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
         MVC   APPCACTN,HALF       APPC COMMAND                                 
         XC    APPCHDR,APPCHDR                                                  
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
*                                  DEBUG MESSAGE                                
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   0(2,R3),=X'0000'                                                 
         SR    RF,RF               DROP CR/LF FROM END OF BUFFER                
         ICM   RF,3,APPCLEN                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         STCM  RF,3,APPCLEN                                                     
         OI    APPCPARM+20,X'80'                                                
         LA    R1,APPCPARM                                                      
         LINK  EPLOC=APPCEPLC                                                   
*                                                                               
ACOM010  LTR   RF,RF                                                            
         BZ    ACOMOK                                                           
         MVC   P+50(9),=C'REG 15 = '                                            
         EDIT  (RF),(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   P+66(8),=C'FLAGS = '                                             
         GOTO1 =V(HEXOUT),DMCB,APPCHDR,P+74,2,=C'TOG'                           
         B     ACOMNO                                                           
*                                                                               
ACOMOK   SR    RC,RC                                                            
ACOMNO   LTR   RC,RC                                                            
ACOMX    XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
COUNT    DS    F                                                                
TESTECB  DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
STIMERID DS    XL4                 FOR TIMERPOPS                                
LENMAX   DS    XL4                                                              
WORK     DS    CL256                                                            
         SPACE 2                                                                
WAITSECS DC    F'1000'                                                          
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
         DS    0F                                                               
EXECBLST DC    X'00',AL3(0)        A(XMAINECB)                                  
         DC    X'80',AL3(0)        A(XSTOPECB)                                  
         SPACE 2                                                                
         DS    0D                                                               
APPCPARM DS    0D                                                               
         DC    A(APPCACTN)                                                      
         DC    A(APPCTOKN)                                                      
         DC    A(APPCDATA)                                                      
         DC    A(APPCEPLC)                                                      
         DC    A(APPCLOGR)                                                      
         DC    X'80000000'         A(ECB) GETS STORED HERE                      
         SPACE 2                                                                
APPCACTN DS    CL4                 ACTION CODE                                  
APPCACOA DC    C'OA  '             ACTION OPEN SEND TO AT&T EASYLINK            
APPCACOS DC    C'OS  '             ACTION OPEN SEND                             
APPCACOR DC    C'OR  '             ACTION OPEN RECEIVE                          
APPCACOI DC    C'OI  '             ACTION OPEN FOR INPUT                        
APPCACSD DC    C'SD  '             ACTION SEND DATA                             
APPCACSF DC    C'SF  '             ACTION SEND FLUSH                            
APPCACSC DC    C'SC  '             ACTION SEND CONFIRMATION                     
APPCACSR DC    C'SR  '             ACTION SEND/RECEIVE                          
APPCACR  DC    C'R   '             ACTION RECEIVE                               
APPCACRC DC    C'RC  '             ACTION RECEIVE/CONFIRM                       
APPCACRS DC    C'RS  '             ACTION RECEIVE/SEND                          
APPCACCP DC    C'CP  '             ACTION CLOSE/PREPARE                         
APPCACC  DC    C'C   '             ACTION CLOSE                                 
APPCTOKN DC    F'0'                TOKEN FOR EASYLINK                           
APPCEPLC DS    CL8                 PROVIDED BY APPC OPEN CALL                   
         DS    0D                                                               
         DC    CL8'APPCDATA'                                                    
APPCDATA DS    0X                  DATA AREA                                    
APPCHDR  DC    XL6'00'             HEADER INFO                                  
APPCMXLN DC    XL2'00'             MAXIMUM LENGTH OF DATA                       
APPCCNUM DC    XL2'00'             CONVERSATION ID NUMBER                       
APPCLEN  DC    XL2'00'             LENGTH OF DATA                               
APPCDTA  DC    1000X'00'           ACTUAL DATA                                  
APPCDLQ  EQU   *-APPCDTA           MAXIMUM DATA LENGTH                          
APPCMAXL EQU   *-APPCLEN           MAXIMUM LENGTH OF DATA                       
APPCMXLQ EQU   *-APPCMXLN          MAXIMUM LENGTH OF DATA FOR RECEIVE           
         SPACE 2                                                                
APPCLOGR DCB   DDNAME=EAPPCLGR,RECFM=FBA,LRECL=133,MACRF=PM,DSORG=PS            
         SPACE 2                                                                
APPCM1   DC    C'*DDS*HERE COMES A REPORT'                                      
APPCM1LQ EQU   (*-APPCM1)+2                                                     
*                                                                               
APPCM2   DC    C'SEND FILE'                                                     
APPCM2LQ EQU   (*-APPCM2)+2                                                     
*                                                                               
APPCM3   DC    C'*DDS*REPORT IS OPENED XXXXX'                                   
APPCM3LQ EQU   (*-APPCM3)+2                                                     
*                                                                               
APPCM4   DC    C'*DDS*END OF REPORT'                                            
APPCM4LQ EQU   (*-APPCM4)+2                                                     
*                                                                               
APPCM5   DC    C'*DDS*REPORT CLOSED'                                            
APPCM5LQ EQU   (*-APPCM5)+2                                                     
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
XIO      DC    10000X'00'          EXFILE I/O AREA                              
*                                                                               
         DS    0D                                                               
         DC    C'EDICTBLK'                                                      
EDICTBLK DC    14336X'00'          EDICT FILE BLOCK                             
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
