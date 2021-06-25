*          DATA SET SPSFM3A    AT LEVEL 083 AS OF 08/11/11                      
*PHASE T2173AA                                                                  
         TITLE 'SPSFM3A<==>T2173A CABLE RECORDS LISTING'                        
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM3A<==>T2173A CABLE RECORDS LISTING.                    *         
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
T2173A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2173A*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         OI    GLSTSTAT,NOSELFLD   NO SELECT FIELD                              
*                                                                               
         CLI   MODE,SETFILE        SET FILE                                     
         BNE   MAIN10                                                           
         BAS   RE,SF                                                            
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS.                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         B     LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*========================== SET FILE ROUTINE =========================*         
SF       NTR1                                                                   
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   SYSDIR,=C'STATION ' SET FILENAME.                                
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,L'CBLKEY         SET KEY LENGTH.                              
         STH   R1,LKEY                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
         LA    R2,CBLMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   CALL,=C'0000T'      USE AS A START-AT POINT,                     
         LA    R2,CBLCALLH          IF THERE IS NO HEADEND INPUT.               
         CLI   5(R2),0                                                          
         BE    VK10                NO HEADEND INPUT.                            
*                                                                               
         BAS   RE,MAKECALL         VALIDATE INPUT AND MAKE CABLE CODE.          
         MVI   CALL+4,C'T'                                                      
*                                                                               
VK10     XC    MSOFILT,MSOFILT     CLEAR POSSIBLE FILTERS                       
         XC    ITFILT,ITFILT                                                    
         MVI   MSOLEN,0                                                         
         MVI   ITLEN,0                                                          
         LA    R2,CBLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         BAS   RE,SETFILT          HOW MANY FILTERS ARE THERE                   
*                                                                               
VK50     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CBLKEY,R4                                                        
         MVI   CBLKTYPE,C'Y'       CABLE RECORDS ARE TYPE-C'Y'.                 
         MVC   CBLKMED,QMED        MEDIA.                                       
         MVC   CBLKCALL,CALL       STATION HEADEND.                             
         MVC   CBLKAGY,AGENCY      AGENCY.                                      
         DROP  R4                                                               
*                                                                               
XVK      MVC   MYSVKEY,KEY                                                      
*                                                                               
         LA    R1,CBLLISTH                                                      
         ST    R1,AFRSTREC                                                      
*                                                                               
         BAS   RE,SF               SET FILE.                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        SET FILTERS                                                            
*                                                                               
SETFILT  NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         LA    R4,8(R2)                                                         
*                                                                               
SF10     SR    R3,R3                                                            
         LA    R5,3(R4)                                                         
         SH    R1,=H'3'            MINUMUM OF 3 FOR LABEL                       
         CLC   0(4,R4),=C'MSO='                                                 
         BNE   SF20                                                             
         LA    R5,1(R5)                                                         
         BCTR  R1,0                                                             
         B     SF30                                                             
*                                                                               
SF20     CLC   0(3,R4),=C'IT='                                                  
         BNE   INVERR                                                           
*                                                                               
SF30     CLI   0(R5),C','                                                       
         BE    SF40                                                             
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,SF30                                                          
*                                                                               
SF40     BCTR  R1,0                DECREMENT FOR THE ','                        
         BCTR  R3,0                                                             
         CLC   0(4,R4),=C'MSO='                                                 
         BNE   SF50                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MSOFILT(0),4(R4)                                                 
         STC   R3,MSOLEN                                                        
         B     SF60                                                             
*                                                                               
SF50     EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ITFILT(0),3(R4)                                                  
         STC   R3,ITLEN                                                         
*                                                                               
SF60     LA    R4,1(R5)            POINT R4 TO AFTER THE ','                    
         LTR   R1,R1                                                            
         BP    SF10                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================== LIST RECORDS ROUTINE =======================*         
*                                                                               
LR       DS    0H                                                               
         LA    R2,CBLMEDIH         WANT CURSOR HERE AFTER LISTING.              
         ST    R2,ACURFORC         LET GENCON KNOW ABOUT IT.                    
*                                                                               
         BAS   RE,SF               SET FILE.                                    
*                                                                               
         OC    KEY(L'CBLKEY),KEY     FIRST TIME THROUGH?                        
         BNZ   *+10                   NO, READ WHERE LEFT OFF.                  
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(2),MYSVKEY   IS 1ST TWO BYTES WHAT WE'RE LOOKING             
         BNE   XLR               FOR?  NO, EXIT LISTING.                        
*                                                                               
         CLC   KEY+(CBLKAGY-CBLKEY)(L'CBLKAGY),MYSVKEY+(CBLKAGY-CBLKEY)         
         BNE   LR10                  TEST IF AGENCY MATCHES.                    
*                                                                               
         L     R4,AIO                                                           
         USING CBLRECD,R4                                                       
         TM    CSYSDACT,X'FF'        DEACTIVATED                                
         BO    LR10                                                             
*                                                                               
         OC    MSOFILT,MSOFILT     IS THERE AN MSO FILTER                       
         BZ    LR80                                                             
         ZIC   R1,MSOLEN           LEN TO COMP                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CSYSMSO(0),MSOFILT                                               
         BNE   LR110                                                            
*                                                                               
LR80     OC    ITFILT,ITFILT       IS THERE AN IT FILTER                        
         BZ    LR90                                                             
         ZIC   R1,ITLEN            LEN TO COMP                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CSYSICN(0),ITFILT                                                
         BNE   LR110                                                            
*                                                                               
         USING LISTD,R2                                                         
LR90     LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    *+8                                                              
         LA    R2,P                                                             
*                                                                               
         MVC   LSTCALL,CBLKCALL    STATION HEADEND.                             
         MVC   LSTCLT,SPACES                                                    
         CLC   CBLKCLT,=C'000'     CLIENT                                       
         BE    *+10                                                             
         MVC   LSTCLT,CBLKCLT                                                   
         MVC   LSTICN,CSYSICN      INTERCONNECT NAME.                           
         MVC   LSTMSO,CSYSMSO      MSO NAME.                                    
         CLC   LSTMSO,SPACES       HAVE MSO NAME?                               
         BH    LR95                YES                                          
         XR    R1,R1                                                            
         ICM   R1,3,CBLKLEN        RECORD LENGTH                                
         CHI   R1,CBLLNQ2          HAVE MSO1 NAME?                              
         BNE   LR95                NO                                           
         CLC   CSYSMSO1,SPACES     HAVE MSO1 NAME?                              
         BNH   LR95                NO                                           
         MVI   LSTMSO,C'*'         INDICATE THAT THIS COMES FROM MSO1           
         MVC   LSTMSO+1(L'LSTMSO-1),CSYSMSO1                                    
*                                                                               
LR95     BAS   RE,GETNAME          GET CABLE NAME                               
         MVC   LSTNAME,CBLNAME     CABLE SYSTEM NAME                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   LR100                                                            
         GOTO1 LISTMON                                                          
         B     LR110                                                            
*                                                                               
LR100    GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR110    B     LR10                                                             
*                                                                               
XLR      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        READ MASTER RECORD TO GET CABLE NAME                                   
*                                                                               
GETNAME  NTR1                                                                   
         MVC   CBLNAME,SPACES                                                   
         MVC   MYKEY,KEY           SAVE CURRENT KEY                             
         LA    R4,MYKEY                                                         
         USING CBLRECD,R4                                                       
*                                                                               
         LA    R2,KEY                                                           
         USING STARECD,R2                                                       
         XC    KEY,KEY                                                          
         MVC   STAKEY,=CL15'0'                                                  
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,CBLKCALL   CALL LETTERS                                 
         MVC   STAKAGY,AGENCY      AGENCY CODE                                  
         MVC   STAKCLT,CBLKCLT     CLIENT EXCEPTION                             
         GOTO1 HIGH                                                             
         CLC   KEY(STAKCLT-STAKEY),KEYSAVE     CMP TO CLT                       
         BNE   GN10                                                             
         L     R2,AIO                                                           
         MVC   CBLNAME,SSYSNAME                                                 
*                                                                               
GN10     MVC   KEY,MYKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
*                                                                               
GNX      B     EXIT                                                             
         EJECT                                                                  
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
CBERR1   MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(L'CBERR1MS),CBERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
CBERR1MS DC    C'** ERROR ** STATION INPUT EXPECTED'                            
*                                                                               
CBERR2   MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(L'CBERR2MS),CBERR2MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
CBERR2MS DC    C'** ERROR ** CABLE STATION IS TOO LONG'                         
         EJECT                                                                  
         LTORG                                                                  
*          DATA SET SPSFM39    AT LEVEL 011 AS OF 01/06/93                      
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,55,REQUESTOR                                                  
         SSPEC H2,55,REPORT                                                     
         SSPEC H2,69,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CABLE LIST'                                              
         SSPEC H2,30,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'STATION'                                                  
         SSPEC H4,9,C'NAME'                                                     
         SSPEC H4,34,C'INTERCONNECT NAME'                                       
         SSPEC H4,55,C'MSO NAME'                                                
         SPACE 1                                                                
         SSPEC H5,1,C'-------'                                                  
         SSPEC H5,9,C'----'                                                     
         SSPEC H5,34,C'-----------------'                                       
         SSPEC H5,55,C'--------'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBAD          DSECT FOR RECORD LISTING.                    
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
MYSVKEY  DS    CL(L'KEY)                                                        
MYKEY    DS    CL(L'KEY)                                                        
CBLNAME  DS    CL24                                                             
CALL     DS    CL5                 CALL LETTERS/CABLE HEADEND                   
MSOFILT  DS    CL15                MSO FILTER                                   
ITFILT   DS    CL20                IT FILTER                                    
MSOLEN   DS    X                   L'INPUT                                      
ITLEN    DS    X                                                                
         EJECT                                                                  
*============================ MY DSECTS =============================*          
*                                                                               
LISTD    DSECT                                                                  
LSTCALL  DS    CL4                 HEADEND                                      
         DS    CL4                                                              
LSTCLT   DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LSTNAME  DS    CL24                CABLE NAME                                   
         DS    CL1                                                              
LSTICN   DS    CL20                INTERCONNECT NAME                            
         DS    CL1                                                              
LSTMSO   DS    CL15                MSO NAME                                     
         EJECT                                                                  
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPSFM3A   08/11/11'                                      
         END                                                                    
