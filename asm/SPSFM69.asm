*          DATA SET SPSFM69    AT LEVEL 003 AS OF 08/11/11                      
*PHASE T21769A                                                                  
         TITLE 'SPSFM69<==>T21769 CABLE RECORDS DIS/CHA'                        
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM69<==>T21769 CABLE RECORDS DIS/CHA.                    *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE CABLE RECORDS.                       *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               GEGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMBA (T217BA) -- LISTING                        *         
*                                                                     *         
*  OUTPUTS: A LIST OF CABLE ('Y'-) RECORDS                            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T21769   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21769*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,SETFILE        SET FILE                                     
         BNE   MAIN10                                                           
         BAS   RE,SF                                                            
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*========================== SET FILE ROUTINE =========================*         
SF       NTR1                                                                   
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
         MVC   SYSDIR,=C'STATION ' SET FILENAME.                                
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,L'CBLKEY         SET KEY LENGTH.                              
         STH   R1,LKEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
         LA    R2,CBMMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,CBMCALLH         DIS/CHA REQUIRES STATION                     
         BAS   RE,MAKECALL                                                      
         MVI   CALL+4,C'T'                                                      
         NI    CBMMSOH+1,X'FF'-X'20'                                            
         NI    CBMITCH+1,X'FF'-X'20'                                            
         CLC   8(4,R2),=C'7000'    LESS THAN 7000?                              
         BL    VK10                YES, PROTECT MSO AND INTERCONNECT            
         CLC   8(4,R2),=C'7500'    BETWEEN 7000 AND 7500?                       
         BNH   VK25                YES                                          
*                                                                               
VK10     OI    CBMMSOH+1,X'20'     PROTECT                                      
         OI    CBMITCH+1,X'20'     PROTECT                                      
*                                                                               
VK25     OI    CBMMSOH+6,X'80'     XMIT                                         
         OI    CBMITCH+6,X'80'     XMIT                                         
*                                                                               
         LA    R2,CBMCLTH                                                       
         CLI   5(R2),0             HAVE A CLIENT?                               
         BE    VK50                NO, DONE                                     
         GOTO1 VALICLT             YES - VALIDATE IT                            
*                                                                               
VK50     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CBLKEY,R4                                                        
         MVI   CBLKTYPE,C'Y'       CABLE RECORDS ARE TYPE-C'Y'.                 
         MVC   CBLKMED,QMED        MEDIA.                                       
         MVC   CBLKCALL,CALL       STATION HEADEND.                             
         MVC   CBLKAGY,AGENCY      AGENCY.                                      
         MVC   CBLKCLT,=C'000'     NO CLIENT                                    
         MVC   CBLKFIL2,=C'000'    DEFAULT VALUE                                
         CLI   CBMCLTH+5,0         HAVE A CLIENT?                               
         BE    XVK                 NO, DONE                                     
         MVC   CBLKCLT,QCLT        AND THEN INCLUDE IT IN THE KEY               
         DROP  R4                                                               
*                                                                               
XVK      BAS   RE,SF               SET FILE.                                    
         B     EXIT                                                             
*                                                                               
*======================== DISPLAY RECORD ROUTINE =====================*         
*                                                                               
DR       DS    0H                                                               
         L     R4,AIO                                                           
         USING CBLRECD,R4                                                       
         XC    CBMITC,CBMITC         CLEAR INTERCONNECT NAME                    
         XC    CBMMSO,CBMMSO         CLEAR MSO NAME                             
         XC    CBMMSO1,CBMMSO1       CLEAR MSO1 NAME                            
         OI    CBMITCH+6,X'80'       TRANSMIT                                   
         OI    CBMMSOH+6,X'80'       TRANSMIT                                   
         OI    CBMMSO1H+6,X'80'      TRANSMIT                                   
         TM    CSYSDACT,X'FF'        DEACTIVATED?                               
         BO    ERRDACT               YES                                        
*                                                                               
         MVC   CBMITC,CSYSICN        INTERCONNECT NAME                          
         MVC   CBMMSO,CSYSMSO        MSO NAME                                   
         SR    R1,R1                                                            
         ICM   R1,3,CBLKLEN          RECORD LENGTH                              
         CHI   R1,CBLLNQ             OLD LENGTH?                                
         BE    DRX                   YES, NO MSO1 NAME                          
         MVC   CBMMSO1,CSYSMSO1                                                 
*                                                                               
DRX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*======================== VALIDATE RECORD ROUTINE ====================*         
*                                                                               
VR       DS    0H                                                               
         L     R4,AIO                                                           
         USING CBLRECD,R4                                                       
         TM    CSYSDACT,X'FF'        DEACTIVATED?                               
         BO    ERRDACT               YES                                        
         CLC   8(4,R2),=C'7000'      LESS THAN 7000?                            
         BL    VR10                  YES                                        
         CLC   8(4,R2),=C'7500'      GREATER THAN 7500?                         
         BH    VR10                  YES                                        
*                                                                               
         MVC   CSYSICN,CBMITC        INTERCONNECT NAME                          
         MVC   CSYSMSO,CBMMSO        MSO NAME                                   
*                                                                               
VR10     MVC   CSYSMSO1,CBMMSO1      MSO1 NAME                                  
         SR    R1,R1                                                            
         LHI   R1,CBLLNQ2            NEW LENGTH                                 
         STCM  R1,3,CBLKLEN                                                     
         DROP  R4                                                               
*                                                                               
VRX      B     DR                    DISPLAY THE RECORD                         
*                                                                               
*============================== MAKEQSTA =============================*         
MAKECALL NTR1                                                                   
*         VALIDATES CBLCALL INPUT...IF VALID, PUT INPUT INTO CALL               
*         ON ENTRY, R2-->CBLCALLH.                                              
*                                                                               
         MVI   ERROR,INVSTAT       ASSUME INVALID CABLE INPUT                   
         TM    4(R2),X'08'         FIRST CHECK IF INPUT IS CABLE                
         BZ    CBERR1               IF NOT, SIGNAL ERROR MESSAGE                
         CLI   5(R2),4             L(CABLE INPUT)<=4                            
         BH    CBERR2               CABLE INPUT TOO LONG ERROR                  
*                                                                               
         LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,ACOMFACS                                                 
         GOTO1 STAVAL,DMCB,(R4)                                                 
         CLI   STBERR,0                                                         
         BNE   ERREXGO                                                          
         MVC   CALL,STBSTA         SET OUTPUT STATION                           
         DROP  R4                                                               
*                                                                               
XMAKECAL B     EXIT                                                             
         EJECT                                                                  
*=========================== ERROR HANDLING ==========================*         
INVERR   MVI   ERROR,INVALID                                                    
         B     ERREXGO                                                          
*                                                                               
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
CBERR1   MVC   CONHEAD(L'CBERR1MS),CBERR1MS                                     
         B     GENERRX                                                          
*                                                                               
CBERR2   MVC   CONHEAD(L'CBERR2MS),CBERR2MS                                     
         B     GENERRX                                                          
*                                                                               
ERRDACT  MVC   CONHEAD(L'DACTERR),DACTERR                                       
*                                                                               
GENERRX  OC    CONHEAD,SPACES                                                   
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
CBERR1MS DC    C'** ERROR ** STATION INPUT EXPECTED'                            
CBERR2MS DC    C'** ERROR ** CABLE STATION IS TOO LONG'                         
DACTERR  DC    C'RECORD IS DEACTIVATED, PLEASE USE MASTER RECORD'               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM6FD          DSECT FOR DIS/CHA                            
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
CALL     DS    CL5                 CALL LETTERS/CABLE HEADEND                   
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPSFM69   08/11/11'                                      
         END                                                                    
