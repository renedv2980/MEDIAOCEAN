*          DATA SET SPREPFXSLK AT LEVEL 017 AS OF 10/02/00                      
*PHASE SPFX02X                                                                  
*INCLUDE PRTREC                                                                 
*===================================================================*           
* THIS PROGRAM MERGES DUPLICATE RECORDS FROM SPREPFXSL3 CONVERSION  *           
*===================================================================*           
         TITLE 'SPFX02 - GENERATE XSPFILE STATION LOCKIN RECORDS'               
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    FXX                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* REQFRST                                                                       
FX00     OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX10     XC    XKEY,XKEY                                                        
         LA    R1,FILEIN                                                        
         LA    R0,P                                                             
         GET   (R1),(0)                                                         
         GOTO1 HEXIN,DMCB,P,XKEY+36,8,0                                         
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FILEINDA,XKEY+36                                                 
* READ THE DUPLICATE RECORD                                                     
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',XKEY+36,XREC,DMWORK              
*                                                                               
         XC    XKEY,XKEY                                                        
         MVC   XKEY,XREC           MOVE DUP KEY                                 
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLC   XKEY(32),XKEYSAVE                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',XKEY+36,ADBUY,DMWORK             
*                                                                               
         L     R8,ADBUY                                                         
         AHI   R8,42               POINT TO FIRST ELEMENT                       
         MVI   UPDFLAG,C'N'                                                     
*                                                                               
FX22     MVI   ELCDLO,X'03'                                                     
         MVI   ELCDHI,X'03'                                                     
         LR    R6,R8                                                            
         BAS   RE,NEXTEL                                                        
         BNE   FX30                                                             
         LR    R8,R6               SAVE ELEMENT POINTER                         
* LOOK FOR ELEMENT IN DUPLICATE RECORD ON FILE                                  
         LA    R6,XREC                                                          
         AHI   R6,42                                                            
*                                                                               
FX24     BAS   RE,NEXTEL                                                        
         BNE   FX26                                                             
         CLC   2(2,R8),2(R6)       MATCH DATE                                   
         BH    FX24                INSERTED ELEM IS HIGH - GO ON                
         BE    FX22                YES - SKIP                                   
* ADD ELEMENT AT 0(R8) TO RECORD                                                
FX26     MVI   UPDFLAG,C'Y'                                                     
         GOTO1 RECUP,DMCB,(C'T',XREC),(R8),(R6)                                 
         B     FX22                                                             
*                                                                               
FX30     CLI   UPDFLAG,C'Y'        TEST RECORD UPDATED                          
         BE    FX32                                                             
* RECORD WASN'T UPDATED                                                         
         GOTO1 HEXOUT,DMCB,XKEY+36,P,4                                          
         MVC   P+10(11),=C'NOT UPDATED'                                         
         GOTO1 HEXOUT,DMCB,FILEINDA,P+23,4                                      
         GOTO1 REPORT                                                           
* PRINT RECORD ON FILE                                                          
         MVC   P(8),=C'FILE REC'                                                
         GOTO1 REPORT                                                           
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(42,32),PRINT,HEXOUT                
         GOTO1 REPORT                                                           
* PRINT DUPLICATE RECORD                                                        
         MVC   P(8),=C'DUP REC '                                                
         GOTO1 REPORT                                                           
         GOTO1 =V(PRTREC),DMCB,(C'E',XREC),(42,32),PRINT,HEXOUT                 
         GOTO1 REPORT                                                           
         B     FX10                                                             
*                                                                               
FX32     CLI   RCWRITE,C'Y'                                                     
         BNE   FX34                                                             
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFILE',XKEY+36,XREC,DMWORK              
*                                                                               
FX34     GOTO1 HEXOUT,DMCB,XKEY+36,P,4                                          
         MVC   P+10(11),=C'MERGED WITH'                                         
         GOTO1 HEXOUT,DMCB,FILEINDA,P+23,4                                      
         GOTO1 REPORT                                                           
         GOTO1 =V(PRTREC),DMCB,(C'E',XREC),(42,32),PRINT,HEXOUT                 
         GOTO1 REPORT                                                           
         AP    FIXCOUNT,=P'1'                                                   
         B     FX10                                                             
*                                                                               
FXX      CLOSE FILEIN                                                           
         OI    FIXCOUNT+3,X'0F'                                                 
         UNPK  P(6),FIXCOUNT                                                    
         MVC   P+8(14),=C'RECORDS FIXED'                                        
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
FILEINDA DS    F                                                                
FIXCOUNT DC    PL4'0'                                                           
UPDFLAG  DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DS    0D                                                               
ELEM     DS    XL64                                                             
         DS    0D                                                               
         DC    CL8'**XKEY**'                                                    
XKEY     DS    XL48                                                             
XKEYSAVE DS    XL48                                                             
         DC    C'**XREC**'                                                      
XREC     DS    4000C                                                            
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,LRECL=133,              X        
               MACRF=GM,EODAD=FXX                                               
         EJECT                                                                  
       ++INCLUDE SPGENXLK                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPFXSLK10/02/00'                                      
         END                                                                    
