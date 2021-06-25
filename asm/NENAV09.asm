*          DATA SET NENAV09    AT LEVEL 016 AS OF 04/04/18                      
*          DATA SET NENAV07    AT LEVEL 075 AS OF 09/17/07                      
*PHASE T31809A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31809   TITLE 'F NAV09 - STEWARD PHEADER REQUEST PROGRAM'                      
T31809   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV09**,RA                                                    
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
* DID WE GO TO THE BUY ALREADY                                                  
*                                                                               
*SVGLOBSW MEANINGS  NULL=FIRST PASS GET FIRST TSAR RECORD CALL WRI              
*                   C'B'=SEND DATA TO THE PC                                    
*                   C'E'=NO MARE TSAR RECORDS EXIT                              
         CLI   SVGLOBSW,C'B'                                                    
         BNE   MAIN40                                                           
         BAS   RE,SENDATA                                                       
         B     EXIT                                                             
*                                                                               
*  SEND DATA TO THE WRITER SYSTEM                                               
*                                                                               
MAIN40   GOTO1 VALIMED                                                          
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
*                                                                               
         BAS   RE,CALLWRIT                                                      
         B     EXIT                                                             
*==================================================================*            
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO NET WRITER SYSTEM *            
*==================================================================*            
         SPACE 1                                                                
CALLWRIT NTR1                                                                   
         MVI   SVXFROV,X'09'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO RETURN FROM REQUEST            
*                                                                               
         MVI   TSACTN,TSASAV        SAVE TSAR                                   
         BAS   RE,CALLTSAR                                                      
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
         L     R3,ANETBLK                                                       
         USING PHEADST,R3                                                       
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                              
         MVI   DMCB+8,1            PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
         MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         BE    *+6                                                              
         DC    H'0'                                                             
* PASS PAGE AND MONITOR NUMBER THROUGH GLOBBER                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY1                            
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'WRI'                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      B     EXIT                                                             
*                                                                               
GLBERR   DC    H'0'                                                             
         DROP  R3,R1                                                            
         EJECT                                                                  
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDATA  NTR1                                                                   
*                                                                               
         LHI   R1,X'79'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF REQUEST WENT THROUGH                                                
*                                                                               
         MVI   BYTE,C'Y'                                                        
         OC    WORK(4),WORK                                                     
         BNZ   *+8                                                              
         MVI   BYTE,C'N'                                                        
*                                                                               
         LA    R4,BYTE                                                          
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
         SPACE 1                                                                
CALLTSR2 LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VTSAR,TSARBLK                                                    
         PRINT NOGEN                                                            
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
EXIT     XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK               LOCAL WORKING STORAGE                       
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
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016NENAV09   04/04/18'                                      
         END                                                                    
