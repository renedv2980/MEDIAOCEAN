*          DATA SET SRRCT00    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T10E00A                                                                  
*                                                                               
* TCLE 001 09JUL96 NEW PROGRAM - RESETS UNIVERSE TABLE                          
*                                                                               
         TITLE '$RESTAB - GENERALISED CORE-RES TABLE RESETTER'                  
REST     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL,*$RESTAB,RA,RR=R2,CLEAR=YES                                 
         USING WRKD,RC             RC=A(W/S)                                    
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         ST    R2,RELO                                                          
         ST    R1,APARMS                                                        
         MVC   PARMS(PARMSL),0(R1)                                              
         USING TWAD,R8             R8=A(TWA)                                    
         L     R8,ATWA                                                          
*                                                                               
* LOCATE COMFACS ROUTINES                                                       
*                                                                               
         L     RE,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(,RE)                                         
         ST    RF,ACALLOV                                                       
         L     RF,CSWITCH-COMFACSD(,RE)                                         
         ST    RF,ASWITCH                                                       
*                                                                               
* LOCATE CORERES ROUTINES                                                       
*                                                                               
         GOTO1 ACALLOV,DMCB,0,X'D9000A0D' GET T00A0D SQUASHER                   
         ICM   RF,15,0(R1)         RF=A(SQUASHER)                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ASQUASH                                                       
*                                                                               
* SUNDRY INITIALISATION                                                         
*                                                                               
         MVC   SRVMSG,SPACES                                                    
         EJECT                                                                  
* VALIDATE TABLE                                                                
*                                                                               
VALTAB   LA    R2,SRVP1H           POINT TO TABLE FIELD                         
         CLI   5(R2),8             FIELD LENGTH                                 
         BH    EFTL                FIELD TOO LONG                               
         XR    R1,R1                                                            
         ICM   R1,1,5(R2)          FIELD LENGTH                                 
         BZ    EMIF                FIELD MISSING                                
         BCTR  R1,0                                                             
         LA    R3,TBLTAB           LOOK FOR TABLE IN TABLE OF TABLES            
         USING TBLTABD,R3                                                       
VALTAB10 EX    R1,VALTABCL                                                      
         BE    VALTAB20                                                         
         LA    R3,TBLTABLQ(,R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BNE   VALTAB10                                                         
         B     ETBL                UNKNOWN TABLE                                
VALTABCL CLC   SRVP1(0),TBLTNAME   EXECUTED                                     
VALTAB20 ST    R3,ATABENT                                                       
         XR    R1,R1                                                            
         ICM   R1,3,TBLTAAT        PICK UP DISP TO ACTION TABLE                 
         AR    R1,RB                                                            
         LR    R4,R1               R4=ACTION TABLE FOR THIS TABLE               
         USING TBLACTD,R4                                                       
*                                                                               
* VALIDATE ACTION                                                               
*                                                                               
VALACT   LA    R2,SRVP2H           POINT TO ACTION FIELD                        
         CLI   5(R2),8             FIELD LENGTH                                 
         BH    EFTL                FIELD TOO LONG                               
         XR    R1,R1                                                            
         ICM   R1,1,5(R2)          FIELD LENGTH                                 
         BZ    EMIF                FIELD MISSING                                
         BCTR  R1,0                                                             
VALACT10 EX    R1,VALACTCL                                                      
         BE    VALACT20                                                         
         LA    R4,TBLACTLQ(,R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   VALACT10                                                         
         B     EACT                UNKNOWN ACTION                               
VALACTCL CLC   SRVP2(0),TBLANAME                                                
VALACT20 ST    R4,AACTENT                                                       
         XR    R1,R1                                                            
         ICM   R1,3,TBLAARTN       PICK UP DISP TO ACTION HANDLER               
         AR    R1,RB                                                            
         LR    RF,R1               RF=ADDR ROUTINE FOR ACTION                   
         BR    RF                  GO TO IT                                     
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
* ERROR EXITS ETC.                                                              
*                                                                               
EFTL     MVC   MSGTXT(L'MFTL),MFTL                                              
         B     ESETERR                                                          
EMIF     MVC   MSGTXT(L'MMIF),MMIF                                              
         B     ESETERR                                                          
ETBL     MVC   MSGTXT(L'MTBL),MTBL                                              
         B     ESETERR                                                          
EACT     MVC   MSGTXT(L'MACT),MACT                                              
         B     ESETERR                                                          
EUKE     MVC   MSGTXT(L'MUKE),MUKE                                              
         B     ESETERR                                                          
EOK      MVC   MSGTXT(L'MOK),MOK                                                
         B     ESETOK                                                           
*                                                                               
ESETERR  MVC   MSGSEV,=CL8'ERROR -'                                             
         B     ESETTAB                                                          
ESETOK   MVC   MSGSEV,=CL8'OK -'                                                
ESETTAB  ICM   R1,15,ATABENT                                                    
         BZ    ESETSQU                                                          
         MVI   MSGDASH,C'-'                                                     
         MVC   MSGTAB,TBLTNAME-TBLTABD(R1)                                      
         MVC   MSGTABN,=CL5'TABLE'                                              
         ICM   R1,15,AACTENT                                                    
         BZ    ESETSQU                                                          
         MVC   MSGACT,TBLANAME-TBLACTD(R1)                                      
ESETSQU  LA    R0,L'MSG                                                         
         GOTO1 ASQUASH,DMCB,MSG,(R0)                                            
ESETMSG  MVC   SRVMSG,MSG                                                       
         OI    6(R2),X'40'         SET CURSOR                                   
*                                                                               
EXIT     XMOD1                                                                  
*                                                                               
PACHAREA DC    32S(*)                                                           
         EJECT                                                                  
* HANDLE UNIVERSE TABLE RESET ACTION                                            
*                                                                               
*&&UK                                                                           
RTNUNRES LA    R2,SRVP1H           POINT TO TABLE FIELD FOR ERRORS              
         GOTO1 ACALLOV,DMCB,0,X'D9000A3C' GET T00A3C GETUNV                     
         ICM   RF,15,0(R1)         RF=A(GETUNV)                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),=CL8'=UNRESET' CALL GETUNV TO RESET                    
         CLI   0(R1),0                                                          
         BE    EOK                                                              
RTNUN10  CLI   0(R1),1                                                          
         BNE   RTNUN11                                                          
         MVC   MSGTXT(L'MUNNIN),MUNNIN                                          
         B     ESETOK                                                           
RTNUN11  CLI   0(R1),2                                                          
         BNE   EUKE                                                             
         MVC   MSGTXT(L'MUNUIP),MUNUIP                                          
         B     ESETERR                                                          
*&&                                                                             
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
*        TABLE OF VALID TABLES                                                  
*                                                                               
TBLTAB   DS    0D                                                               
*&&UK                                                                           
         DC    CL8'UNIVERSE',AL2(TBLUNACT-REST)                                 
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
*        TABLES OF VALID ACTIONS FOR EACH TABLE                                 
*                                                                               
*        UNIVERSE TABLE ACTIONS                                                 
*                                                                               
TBLUNACT DS    0D                                                               
*&&UK                                                                           
         DC    CL8'RESET   ',AL2(RTNUNRES-REST)                                 
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
MOK      DC    C'SUCCESSFUL'                                                    
MFTL     DC    C'INPUT TOO LONG'                                                
MMIF     DC    C'MISSING INPUT'                                                 
MTBL     DC    C'INVALID TABLE'                                                 
MACT     DC    C'INVALID ACTION FOR TABLE'                                      
MUKE     DC    C'UNKNOWN ERROR'                                                 
*                                                                               
*&&UK                                                                           
MUNNIN   DC    C'NOT YET INITIALISED'                                           
MUNUIP   DC    C'LOAD/RESET ALREADY IN PROGRESS'                                
*&&                                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR TABLES                                                 
*                                                                               
TBLTABD  DSECT                     TABLE TABLE                                  
TBLTNAME DS    CL8                 TABLE NAME                                   
TBLTAAT  DS    CL2                 DISP TO ACTION TABLE (FROM RB)               
TBLTABLQ EQU   *-TBLTABD                                                        
*                                                                               
TBLACTD  DSECT                     ACTION TABLE                                 
TBLANAME DS    CL8                 ACTION NAME                                  
TBLAARTN DS    CL2                 DISP TO ACTION HANDLER (FROM RB)             
TBLACTLQ EQU   *-TBLACTD                                                        
*                                                                               
*              DSECT TO COVER W/S                                               
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
RELO     DS    F                   RELOCATION FACTOR                            
SAVERE   DS    F                                                                
*                                                                               
APARMS   DS    A                   ADDRESS OF INPUT PARM LIST                   
PARMS    DS    0A                  COPY OF INPUT PARM LIST                      
ASYSFAC  DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ACOMFACS DS    A                                                                
ASELIST  DS    A                                                                
ATWA     DS    A                                                                
APHASEMP DS    A                                                                
ATIOB    DS    A                                                                
PARMSL   EQU   *-PARMS                                                          
*                                                                               
ACALLOV  DS    A                   A(CALLOV)                                    
ASWITCH  DS    A                   A(SWITCH)                                    
ASQUASH  DS    A                   A(SQUASH)                                    
*                                                                               
ATABENT  DS    A                   A(TABLE TABLE ENTRY)                         
AACTENT  DS    A                   A(TABLE ACTION TABLE ENTRY)                  
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
TEMP     DS    CL80                                                             
*                                                                               
SPACES   DS    CL80                                                             
*                                                                               
MSG      DS    0CL80                                                            
MSGHDR   DS    0CL34                                                            
MSGSEV   DS    CL8                                                              
MSGTAB   DS    CL8                                                              
         DS    C                                                                
MSGTABN  DS    CL5                                                              
         DS    C                                                                
MSGACT   DS    CL8                                                              
         DS    C                                                                
MSGDASH  DS    C                                                                
         DS    C                                                                
MSGTXT   DS    CL(80-34)                                                        
*                                                                               
WRKL     EQU   *-WRKD                                                           
         EJECT                                                                  
TWAD     DSECT                     SAME NAME AS FATWA, SAVES REGISTER           
         DS    CL64                (SEE FATWA)                                  
       ++INCLUDE SRRCTFFD                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
* FADSECTS                                                                      
* SRDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FACHKPT                                                        
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRRCT00   08/22/00'                                      
         END                                                                    
