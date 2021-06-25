*          DATA SET NENAV06A   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NENAV06    AT LEVEL 013 AS OF 06/15/00                      
*          DATA SET NENAV06    AT LEVEL 054 AS OF 05/24/00                      
*PHASE T31806A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31806   TITLE 'NENAV06 - STEWARD/MATCHMAKER - BUY MAINT OVERLAY'               
T31806   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV06**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
* DID WE GO TO THE BUY ALREADY                                                  
         CLI   SVGLOBSW,C'B'                                                    
         BE    EXIT                                                             
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0010'                                                 
         BE    ROUT020                                                          
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES                                                    
*                                                                               
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
ROUT020  GOTO1 VALIMED                                                          
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO SPOT BUY         *             
*=================================================================*             
         SPACE 1                                                                
CALLBUY  NTR1  BASE=*,LABEL=*                                                   
         MVI   SVXFROV,X'06'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO UNITS BOUGHT                   
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
         LA    RE,BUYDATA1                                                      
         USING BUYDATAD,RE                                                      
         MVC   REQCLI,=CL3'BGM'                                                 
         MVC   REQEST,=CL3'002'                                                 
         MVC   REQNET,=CL4'NBC '                                                
         MVC   REQPACK,=CL3'001'                                                
         MVC   REQPCODE,=CL6'ALF   '                                            
         MVC   REQBDATE,=CL6'990607'                                            
         MVC   REQWEEKS,=CL6'3 '                                                
         MVC   REQPWEEK,=CL6'5  '                                               
*        MVC   REQREAS,=CL3'2  '                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BUYDATA1,200,GLVBUY1                      
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'NBU'                                                 
         OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      XIT1                                                                   
*                                                                               
GLBERR   DCHO                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRKA                                                      
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
SVBDELEM DS    CL67                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
*                                                                               
         DS    0D                                                               
BUYDATA1 DS    CL200                                                            
BUYDATAL EQU   *-BUYDATA1                                                       
BUYDATAX EQU   *                                                                
         ORG                                                                    
*                                                                               
BUYDATAD DSECT                                                                  
REQCLI   DS    CL3                                                              
REQEST   DS    CL3                                                              
REQNET   DS    CL4                                                              
REQPACK  DS    CL3                                                              
REQPCODE DS    CL6                                                              
REQBDATE DS    CL6                                                              
REQREAS  DS    CL3                                                              
REQWEEKS DS    CL2                                                              
REQPWEEK DS    CL3                                                              
REQPRD   DS    XL1                                                              
REQDAY   DS    XL1                                                              
REQTIME  DS    XL4                                                              
REQLEN   DS    XL1                                                              
REQACTC  DS    XL4                                                              
REQBTYPE DS    CL1                                                              
REQNTI   DS    XL2                                                              
REQADU   DS    CL1                                                              
REQBILBD DS    CL1                                                              
REQCOMNT DS    CL120                                                            
REQDEMOS DS    CL28                                                             
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
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NENAV06A  05/01/02'                                      
         END                                                                    
