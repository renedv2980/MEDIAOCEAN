*          DATA SET SPMBA02    AT LEVEL 006 AS OF 05/01/02                      
*PHASE SP0102M,+0                                                               
*INCLUDE MBOSS                                                                  
*INCLUDE MBCIRC                                                                 
         TITLE 'SPMBA02- SPOT/MEDIABASE LINK'                                   
SP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDL,**SPMB**,CLEAR=YES                                        
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         USING WORKD,RC                                                         
         CLI   MODE,REQFRST                                                     
         BE    PROCESS                                                          
EXIT     XIT1                                                                   
*                                                                               
PROCESS  DS    0H                                                               
         LA    R7,MBOSSA           MBOSS CONTROL BLOCK                          
         USING MBOSSD,R7                                                        
         LA    R8,MBBAREA          MBPARMS AREA                                 
         USING MBPARMS,R8                                                       
*                                  SET MBOSS INPUTS                             
         MVI   MBOTYPE,MBOINITQ                                                 
         MVI   MBOONOFF,MBOOFFQ                                                 
         MVI   MBOMINNM,3                                                       
         MVC   MBOCFACA,ACOMFACS                                                
         LA    RF,MBSYSF                                                        
         ST    RF,MBOSFACA                                                      
         ST    R8,MBOMPRMA                                                      
         MVI   MBOCTRY,1                                                        
         MVI   MBOLANG,1                                                        
         MVI   MBOFILET,C'A'                                                    
         XC    MBOAGNCY,MBOAGNCY                                                
         MVC   MBOCNFID,CONFID                                                  
         MVC   MBOLOADR,LOADER                                                  
         MVC   MBORECUP,RECUP                                                   
         MVC   MBOUTL,UTL                                                       
         MVC   MBOUSER,RCORIGID                                                 
*                                                                               
         GOTO1 =V(MBOSS),DMCB,MBOSSD                                            
         CLI   MBOERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,QAREA+29         MEDABASE NUMBER FROM REQUEST CARD            
         OC    0(6,R5),=6C'0'                                                   
         PACK  DUB,0(6,R5)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,7,MBNUM                                                       
         XC    MBNUM(3),=X'FFFFFF'   COMPLEMENT                                 
*                                                                               
*                                  DO HEADER DATA LOOKUP                        
*                                  ---------------------                        
         LA    R6,MBLKWKA                                                       
         USING MBLOOKD,R6          MBLOOK CONTROL BLOCK                         
         LA    RE,MBLKWKA                                                       
         LA    RF,L'MBLKWKA                                                     
         XCEF                                                                   
         MVC   MBLMBNUM,MBNUM      MBNUM                                        
         MVC   MBLDATE,TODAYP      DATE                                         
         LA    RF,MBLKTWKA         A(OUTPUT TABLE)                              
         ST    RF,MBLATAB                                                       
         LA    RF,MBSPCWK          A(SPACE DESC)                                
         ST    RF,MBLASPC                                                       
         MVI   MBLDTYP,MBLHDRQ     DATA TYPE = HEADER LOOKUP                    
*                                                                               
         L     RF,MBASFACS                                                      
         L     RF,SMBLOOK-MBFACSD(RF)                                           
         GOTO1 (RF),MBPARMS,MBLOOKD                                             
*                                                                               
         CLI   QAREA+49,C' '       TEST TO DO RATE LOOKUP                       
         BE    MP20                (SPACE DESC IS IN REQUEST CARD)              
*                                                                               
*                                  SPACE RATE LOOKUP                            
*                                  -----------------                            
*                                                                               
         L     RF,MBASFACS         FIRST USE MBSPVAL TO VALIDATE SPACE          
         L     RF,SMBSPVAL-MBFACSD(RF)                                          
         GOTO1 (RF),MBPARMS,('MBPVALQ',0),(18,QAREA+49),MBVTAB,0,0,    X        
               MBSPCWK                                                          
         CLI   MBPERR,0                                                         
         BNE   MP20                                                             
*                                                                               
         MVI   MBLDTYP,MBLIRATQ    DATA TYPE = INSERT RATE                      
         MVI   MBCTYPV,MBAKDSPQ    SET RATE CARD TYPE                           
*                                                                               
         L     RF,MBASFACS                                                      
         L     RF,SMBLOOK-MBFACSD(RF)                                           
         GOTO1 (RF),MBPARMS,MBLOOKD                                             
*                                                                               
*                                  MBCIRC CALL                                  
*                                  -----------                                  
MP20     DS    0H                                                               
         LA    R6,CIRCWA                                                        
         USING MBCIRCD,R6          MBCIRC CONTROL BLOCK                         
         XC    CIRCWA,CIRCWA                                                    
         MVC   MBCMBNUM,MBNUM                                                   
         MVC   MBCDATE,TODAYP                                                   
         LA    RF,CIRCTABA                                                      
         ST    RF,MBCATAB                                                       
*                                                                               
         GOTO1 =V(MBCIRC),MBPARMS,MBCIRCD                                       
*                                                                               
*                                  DUMP MINBLKS, ETC                            
         MVC   P(20),=CL20'HEADER MINBLK'                                       
         GOTO1 REPORT                                                           
         L     R5,MBAMINH                                                       
         LA    R3,MINBLKL                                                       
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'RATE CARD MINBLK'                                    
         GOTO1 REPORT                                                           
         L     R5,MBAMINC                                                       
         LA    R3,MINBLKL                                                       
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MECH CARD MINBLK'                                    
         GOTO1 REPORT                                                           
         L     R5,MBAMINM                                                       
         LA    R3,MINBLKL                                                       
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MBCIRCD'                                             
         GOTO1 REPORT                                                           
         LA    R5,CIRCWA                                                        
         LA    R3,MBCIRCDL                                                      
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MBCTABD'                                             
         GOTO1 REPORT                                                           
         LA    R5,CIRCTABA                                                      
         LA    R3,MBCTABDL                                                      
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'SPACE DESC'                                          
         GOTO1 REPORT                                                           
         LA    R5,MBSPCWK                                                       
         LA    R3,L'MBSPCWK                                                     
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MBLOOKD'                                             
         GOTO1 REPORT                                                           
         LA    R5,MBLKWKA                                                       
         LA    R3,L'MBLKWKA                                                     
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(20),=CL20'MBLOOK TABLE'                                        
         GOTO1 REPORT                                                           
         LA    R5,MBLKTWKA                                                      
         LA    R3,L'MBLKTWKA                                                    
         BAS   RE,DMP                                                           
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,UTL             RESTORE SE NUM                                
         MVC   4(1,RF),MBOSVSE                                                  
*                                                                               
         CLI   QOPT1,C'D'          TEST TO ABEND                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AENDREQ                                                          
*                                                                               
         EJECT                                                                  
DMP      NTR1                                                                   
         MVI   P,0                                                              
         LA    R3,0(R5,R3)         R3= END                                      
DMP2     DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         GOTO1 REPORT                                                           
         LA    R5,0(R5,R4)                                                      
         B     DMP2                                                             
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         DC    X'8000'                                                          
WORKD    DSECT                                                                  
SAVSE    DS    XL1                                                              
MBNUM    DS    XL3                                                              
         DS    0D                                                               
MBBAREA  DS    XL300                                                            
         DS    0D                                                               
MBSYSF   DS    XL200                                                            
         DS    0D                                                               
MBOSSA   DS    XL(MBOSSDL)                                                      
         DS    0D                                                               
CIRCWA   DS    XL(MBCIRCDL)                                                     
         DS    0D                                                               
CIRCTABA DS    40XL(MBCTABDL)                                                   
         DS    0D                                                               
MBLKWKA  DS    XL(MBLOOKDL)                                                     
         DS    0D                                                               
MBLKTWKA DS    XL2000                                                           
         DS    0D                                                               
MBSPCWK  DS    XL50                                                             
         DS    0D                                                               
MBVTAB   DS    XL256                                                            
*                                                                               
WORKDL   EQU   *-WORKD                                                          
*                                                                               
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE MBGENFILE                                                      
       ++INCLUDE MBGLOBEQUS                                                     
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDMBFACS                                                       
         PRINT ON                                                               
       ++INCLUDE MBOSSD                                                         
       ++INCLUDE MBVALPARMS                                                     
       ++INCLUDE MBCIRCD                                                        
       ++INCLUDE MBLOOKD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPMBA02   05/01/02'                                      
         END                                                                    
