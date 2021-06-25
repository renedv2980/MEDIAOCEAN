*          DATA SET NENAV12    AT LEVEL 076 AS OF 03/30/18                      
*PHASE T31812A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31812   TITLE 'F NAV12 - STEWARD/MATCHMAKER - COPYU OVERLAY'                   
T31812   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV12**,RA                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
*  RESTORE THE TSAR BLOCK                                                       
*                                                                               
         CLI   SVRSTSAV,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSARES        RESTORE TSAR                                
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'R'                                                    
*                                                                               
         CLI   SVGLOBSW,C'B'                                                    
         BE    ROUT040                                                          
*                                                                               
* DID WE GO TO THE BUY ALREADY                                                  
*                                                                               
*SVGLOBSW MEANINGS  NULL=FIRST PASS GET FIRST TSAR RECORD CALL NBUY             
*                   C'B'=SEND DATA TO THE PC                                    
*                   C'E'=NO MARE TSAR RECORDS EXIT                              
*                                                                               
*                                                                               
*  SEND DATA TO THE BUY (FIRST TIME THROUGH)                                    
*                                                                               
         GOTO1 VALIMED                                                          
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
*                                                                               
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
ROUT040  BAS   RE,SENDATA           SEND OUT DATA TO FALINK                     
*                                                                               
         L     RF,ANETBLK                                                       
         MVC   0(1,RF),SVTSRKEY                                                 
         MVI   TSACTN,TSARDH        READ HIGH                                   
         BAS   RE,CALLTSAR          REPOSITION POINTER                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO SPOT BUY         *             
*=================================================================*             
         SPACE 1                                                                
CALLBUY  NTR1                                                                   
         MVI   SVXFROV,X'12'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO UNITS BOUGHT                   
*                                                                               
         CLI   SVRSTSAV,C'R'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSASAV        SAVE TSAR                                   
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'S'                                                    
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
CB20     L     R3,ANETBLK                                                       
         USING CPYUPLDD,R3                                                      
         MVC   SVTSRKEY,0(R3)                                                   
*                                                                               
***      XC    DMCB(24),DMCB                                                    
***      L     RE,ATWA                                                          
***      MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                              
***      MVI   DMCB+8,1            PAGE NUMBER                                  
***      MVI   DMCB+9,0                                                         
***      MVC   DMCB+20(2),=C'L='                                                
***      MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
***      MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
***      GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
***      CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
***      BE    *+6                                                              
***      DC    H'0'                                                             
* DO WSSVR CALL YTO PASS INFO TO THE BUY SYSTEM                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,=CL4'NNAV'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,ANETBLK                                                  
         LHI   R0,RUPLEN                                                        
         STCM  R0,3,FAWSLEN                                                     
******   MVC   FAWSLEN,=H'2000'                                                 
         GOTOR VWSSVRS,FAWSSVRD                                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
* PASS PAGE AND MONITOR NUMBER THROUGH GLOBBER                                  
         MVC   WORK,=CL4'NNAV'                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY1                            
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLVXFRSY,R6                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'NBU'                                                 
* SET DIALOGUE PARAMETERS                                                       
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         OC    SVSESSNS,SVSESSNS    FIRST TIME                                  
         BZ    CALLB050                                                         
         MVC   GLVXSESR(2),SVSESSNS                                             
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         B     CALLB100                                                         
* GET CURRENT SESSION NUMBER                                                    
CALLB050 OI    GLVXFLG1,GLV1SIDR                                                
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R1,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R1                                                          
         MVC   SVSESSNS(1),TSESSION                                             
         MVC   GLVXSESR(2),SVSESSNS                                             
         DROP  R1,R3                                                            
*                                                                               
*******  OI    GLVXFLG1,GLV1SEPS+GLV1SIDR                                       
CALLB100 GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      B     EXIT                                                             
*                                                                               
GLBERR   DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDATA  NTR1                                                                   
*                                                                               
         L     R3,AIO3                                                          
         USING DRAFRECD,R3                                                      
*                                                                               
         LHI   R1,X'29'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   BUYERRSW,C'Y'                                                    
         BNE   SND040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,ERMSSEQN                                                      
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERNUMBR                                                       
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERNUMBR,WORK),(X'FF',DMCB),(7,0)                
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXTRA                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         B     SNDEX                                                            
*                                                                               
SND040   LA    R4,DRFSEQN           SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFCOPY           NUMBER OF UNITS COPIED                      
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDEX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
CALLTSAR LR    R0,RE                                                            
***********                                                                     
*****    LA    R1,SVTSSVRS                                                      
*****    CLI   0(R1),0                                                          
*****    BE    *+12                                                             
*****    LA    R1,1(R1)                                                         
*****    B     *-12                 RESTORE TSAR                                
*****    MVC   0(1,R1),TSACTN                                                   
***********                                                                     
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
CALBUYEX XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
*                                                                               
WSVRBLK  DS    XL(FAWSSVRL)        ** WSSVR CONTROL BLOCK                       
*                                                                               
         DS    0D                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAUTL                                                          
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076NENAV12   03/30/18'                                      
         END                                                                    
