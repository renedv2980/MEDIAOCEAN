*          DATA SET SPSFM38    AT LEVEL 025 AS OF 03/09/04                      
*PHASE T21738A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM38<==>T21738 SPOT REP RECORDS LISTING.                 *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE SPOT STATION-FILES.                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMD8 (T217D8) -- LISTING                        *         
*                                                                     *         
*  OUTPUTS: A LIST OF REP RECORDS                                     *         
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
         TITLE 'SPSFM38<==>T21738 SPOT REP RECORDS LISTING'                     
***********************************************************************         
T21738   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21738*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
*        GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
*        USING FACTSD,R3                                                        
*        L     R3,DMCB                                                          
*        TM    FATSTAT6,X'80'      USING STEREO?                                
*        BNO   MAIN05                                                           
*        OI    RPLNAMHH+1,X'FF'    NOP 'NAME' FIELD                             
*        OI    RPLNAMHH+6,X'80'    XMIT                                         
*        OI    RPLNAMEH+1,X'FF'    NOP UNPROT NAME FIELD                        
*        OI    RPLNAMEH+6,X'80'    XMIT                                         
*                                                                               
MAIN05   CLI   MODE,SETFILE        SET FILE                                     
         BNE   MAIN10                                                           
         BAS   RE,SF                                                            
         B     EXIT                                                             
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
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== SET FILE ROUTINE =========================*         
SF       NTR1                                                                   
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   SYSDIR,=C'STATION ' SET FILENAME.                                
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,REPKEYLQ         SET KEYLENGTH.                               
         STH   R1,LKEY                                                          
*                                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                  SET TO READ AGENCY RECORDS WHEN              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   LKEY,=H'13'         L(AGENCY RECORD)=13.                         
         MVI   USEIO,C'N'                                                       
*                                                                               
*--------------------------- MEDIA FIELD -----------------------------*         
*                                                                               
         LA    R2,RPLMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
VK10     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         MVI   REPKTYPE,C'R'       REP RECORDS ARE TYPE-'R'.                    
         MVC   REPKMED,QMED        MEDIA.                                       
         ZIC   R1,RPLREPH+5                                                     
         LTR   R1,R1                                                            
         BZ    VK20                                                             
         BCTR  R1,0                                                             
         EXMVC R1,REPKREP,RPLREP   REP-CODE.                                    
VK20     MVC   REPKAGY,AGENCY      AGENCY.                                      
         MVC   MYSVKEY(REPKEYLQ),KEY    SAVE THE KEY AROUND.                    
*                                                                               
         CLI   RPLNAMEH+5,0        LIST BY NAME                                 
         BE    XVK                                                              
         LA    R2,RPLNAMEH                                                      
         CLI   RPLREPH+5,0         THEN CAN'T SPECIFY REP CODE                  
         BNE   ERRNAME             CAN'T SPECIFY BOTH REP CODE AND NAME         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPRECD,R4                                                       
         MVI   REBKTYPE,C'B'       REP RECORDS BY NAME -'B'.                    
         MVC   REBKAGY,AGENCY      AGENCY.                                      
         MVC   REBKNAME,RPLNAME                                                 
         MVC   MYSVKEY(REPKEYLQ),KEY    SAVE THE KEY AROUND.                    
         DROP  R4                                                               
*                                                                               
XVK      BAS   RE,SF                                                            
         BAS   RE,TESTSEL                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*============================== TESTSEL ==============================*         
*                                                                               
TESTSEL  NTR1                                                                   
*         TESTS THE SELECT FIELD, AND DOESN'T ALLOW CHANGES TO                  
*          BE MADE TO MEDIAS 'C' & 'N' OF CANADIAN AGENCIES.                    
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY.                    
         BNE   XTESTSEL             EXIT IF NOT CANADIAN.                       
*                                                                               
         CLI   QMED,C'C'           MEDIA 'C'?                                   
         BE    TESTSEL1                                                         
         CLI   QMED,C'N'           MEDIA 'N'?                                   
         BNE   XTESTSEL                                                         
*                                                                               
TESTSEL1 LA    R2,RPLSELH          R2-->FIRST SELECT FIELD.                     
         LA    RF,RPLTAGH          RF-->LAST FIELD.                             
*                                                                               
TESTSEL2 ZIC   R1,0(R2)                                                         
         CLI   8(R2),C'C'          CHANGE FROM LIST SCREEN?                     
         BE    SLERR1                                                           
         CLI   9(R2),C'C'                                                       
         BE    SLERR1                                                           
         CLI   10(R2),C'C'                                                      
         BE    SLERR1                                                           
*                                                                               
** THIS SELECT FIELD WAS OK                                                     
*                                                                               
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    RF,R2                                                            
         BH    TESTSEL2                                                         
*                                                                               
XTESTSEL B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== LIST RECORDS ROUTINE =======================*         
*                                                                               
LR       DS    0H                                                               
*                                                                               
*------------------------ ADMINISTRATIVE WORK ------------------------*         
*                                                                               
         BAS   RE,SF                                                            
*        CLI   MODE,LISTRECS                                                    
*        BNE   LR05                                                             
         CLI   RPLNAMEH+5,0        LIST BY NAME                                 
         BNE   LR200                                                            
*                                                                               
LR05     OC    KEY(REPKEYLQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LRRDHI              NO, READ WHERE LEFT OFF.                     
*                                                                               
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
LRRDHI   GOTO1 HIGH                                                             
*                                                                               
LR10     CLC   KEY(REPKREP-REPRECD),MYSVKEY   IS 1ST TWO BYTES                  
         BNE   XLR                   WHAT WE'RE LOOKING FOR?                    
*                                     NO, EXIT LISTING.                         
         CLC   KEY+(REPKAGY-REPRECD)(L'REPKAGY),MYSVKEY+(REPKAGY-REPREC+        
               D)                                                               
         BNE   LR100               AGENCY DOESN'T MATCH...NEXT RECORD.          
*                                                                               
*----------------------- PUT DATA ON LIST-LINE -----------------------*         
*                                                                               
         L     R4,AIO                                                           
         USING REPRECD,R4                                                       
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSTREP(L'REPKREP),REPKREP       REP CODE.                        
         MVC   LSTNAME,RNAME                   NAME                             
         MVC   LSTCITY,R2LINE                  CITY.                            
         MVC   LSTSTTE,R3LINE                  STATE                            
         MVC   LSTZIP,RBIGZIP                  ZIP CODE.                        
         CLC   RBIGZIP,SPACES                  IF NO ZIP CODE                   
         BH    LR80                                                             
         CLC   RZIP,=C'CANAD'                  AND THIS IS CANADA               
         BNE   *+10                                                             
         MVC   LSTZIP(5),=C'CANAD'             OUTPUT CANADA                    
         DROP  R4                                                               
*                                                                               
LR80     CLI   MODE,LISTRECS                                                    
         BNE   LR90                                                             
         GOTO1 LISTMON                                                          
         B     LR100                                                            
*                                                                               
LR90     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR100    GOTO1 SEQ                                                              
         B     LR10                                                             
*                                                                               
* LIST BY REP NAME                                                              
*                                                                               
LR200    OC    KEY(REPKEYLQ),KEY   FIRST TIME THROUGH?                          
         BNZ   *+10                NO, READ WHERE LEFT OFF.                     
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
         GOTO1 HIGH                                                             
         B     LR210                                                            
LR200SEQ GOTO1 SEQ                                                              
*                                                                               
LR210    L     R4,AIO                                                           
         USING REPRECD,R4                                                       
         CLC   REBKEY(3),MYSVKEY   SAME TYPE AND AGENCY                         
         BNE   XLR                 NO, DONE                                     
         CLC   QMED,REBKMED        SAME MED                                     
         BNE   LR200SEQ                                                         
*                                                                               
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSTREP,REBKREP                  REP CODE.                        
         MVC   LSTNAME,REBNAME                 NAME                             
         DROP  R4                                                               
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   LR220                                                            
         GOTO1 LISTMON                                                          
         B     LR200SEQ                                                         
*                                                                               
LR220    GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR200SEQ                                                         
*                                                                               
XLR      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== ERROR HANDLING ==========================*         
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
SLERR1   MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'SLERR1MS),SLERR1MS                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
SLERR1MS DC    C'** ERROR ** CANNOT CHANGE MEDIA C/N'                           
ERRNAME  MVC   CONHEAD,BLANKS                                                   
         MVC   CONHEAD(L'ERRNAMSG),ERRNAMSG                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
ERRNAMSG DC    C'** ERROR ** CANNOT SPECIFY BOTH REP CODE AND NAME'             
***********************************************************************         
         SPACE 4                                                                
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
         DS    0F                                                               
ZEROES   DC    (L'KEY)CL1'0'                                                    
BLANKS   DC    CL132' '                                                         
***********************************************************************         
         SPACE 4                                                                
**********************************************************************          
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'REP LIST'                                                
         SSPEC H2,30,C'--------'                                                
         SPACE 1                                                                
         SSPEC H4,1,C'REP'                                                      
         SSPEC H4,6,C'NAME'                                                     
         SSPEC H4,31,C'CITY'                                                    
         SSPEC H4,58,C'ST'                                                      
         SSPEC H4,64,C'ZIP CODE'                                                
         SPACE 1                                                                
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,6,C'----'                                                     
         SSPEC H5,31,C'----'                                                    
         SSPEC H5,58,C'--'                                                      
         SSPEC H5,64,C'--------'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
*======================== REP RECORDS DSECT =========================*          
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*       ++INCLUDE DDBIGBOX                                                      
*       ++INCLUDE DDCOMFACS                                                     
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE FASYSFAC                                                      
*       ++INCLUDE FAPGMLST                                                      
*       ++INCLUDE FASELIST                                                      
*       ++INCLUDE FAFACTS                                                       
*       ++INCLUDE FALANG                                                        
*       ++INCLUDE FATIOB                                                        
*       ++INCLUDE FASYSLSTD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*=============================== TWA ================================*          
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD8D          DSECT FOR RECORD LISTING.                    
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= SPSFM WORK AREA ==========================*          
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
MYSVKEY  DS    CL(L'KEY)                                                        
*                                                                               
RELO     DS    F                   RELOCATION FACTOR.                           
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*============================ MY DSECTS =============================*          
*                                                                               
*----------------------- ON-SCREEN LIST LINE ------------------------*          
*                                                                               
LISTD    DSECT                                                                  
LSTREP   DS    CL3                 REP.                                         
         DS    CL2                                                              
LSTNAME  DS    CL22                REP NAME.                                    
         DS    CL3                                                              
LSTCITY  DS    CL24                CITY.                                        
         DS    CL3                                                              
LSTSTTE  DS    CL3                 STATE.                                       
         DS    CL3                                                              
LSTZIP   DS    CL10                ZIP CODE.                                    
**********************************************************************          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPSFM38   03/09/04'                                      
         END                                                                    
