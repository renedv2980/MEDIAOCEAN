*          DATA SET SPSFM32    AT LEVEL 026 AS OF 08/11/11                      
*PHASE T21732A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM32<==>T21732 SPOT ADDRESS RECORDS LISTING.             *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE SPOT STATION-FILES.                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMA2 (T217A2) -- LISTING                        *         
*                                                                     *         
*  OUTPUTS: A LIST OF ADDRESS RECORDS                                 *         
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
         TITLE 'SPSFM32<==>T21732 SPOT ADDRESS RECORDS LISTING'                 
***********************************************************************         
T21732   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21732*,R7,RR=R3                                              
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
***                                                                             
* MAKE SURE GENCON HONORS SECURITY. IF ID IS NOT ALLOWED ACTION CHANGE          
* AND USER SELECTS WITH A 'C' FROM A LIST, THIS BIT WILL INSURE THAT            
* SECURITY IS HONORED AKAT 8/18/04                                              
***                                                                             
         OI    GENSTAT3,OKVALSEL   HONOR SECURITY!                              
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
         LA    R1,ADDKEYLQ         SET KEYLENGTH.                               
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
         LA    R2,ADLMEDH          CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
*                                                                               
         MVI   ERROR,INVMED        ASSUME INVALID MEDIA ERROR                   
         CLI   ADLMED,C'*'         TEST MEDIA 'ALL'                             
         BNE   VK20                                                             
         MVI   QMED,C'T'           FIX QMED = 'T' FOR MEDIA = 'ALL'             
         B     VK40                                                             
*                                                                               
VK20     GOTO1 VALIMED                                                          
         B     VK30                                                             
*                                                                               
*-------------------------- STATION FIELD ----------------------------*         
*                                                                               
** MEDIA = ALL                                                                  
VK40     XC    QSTANEW,QSTANEW     ASSUME NO STATION INPUT                      
         MVC   QSTANEW(3),=C'BOX'  START AT FOR MEDIA = ALL                     
         LA    R2,ADLSTAH                                                       
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VK10                 NO INPUT, SO BUILD KEY                      
         MVI   ERROR,INVSTAT       ASSUME INVALID STATION ERROR                 
*                                                                               
         CLI   5(R2),4             CHECK FOR                                    
         BH    ERREXGO              MAX L'INPUT = 4                             
         CLC   ADLSTA(3),=C'BOX'                                                
         BNE   ERREXGO                                                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,QSTANEW,ADLSTA                                                
         B     VK10                                                             
*                                                                               
** MEDIA <> ALL                                                                 
VK30     XC    QSTANEW,QSTANEW     ASSUME NO STATION INPUT                      
         LA    R2,ADLSTAH          STATION IS NOT MANDATORY, BUT IF IT          
         CLI   5(R2),0              IS INPUTTED, THEN VALIDATE ITS              
         BE    VK10                 SYNTAX                                      
*                                                                               
         MVI   ERROR,INVSTAT       ASSUME INVALID STATION ERROR                 
         CLI   8(R2),C'0'          IF THE FIRST CHAR IS A DIGIT                 
         BL    VK5                                                              
         CLI   8(R2),C'9'          THEN THIS IS CABLE                           
         BH    VK5                                                              
         CLI   5(R2),4             MAX FOR CBLE STATION IS 4 (DIGITS)           
         BH    ERREXGO                                                          
*                                                                               
VK5      LA    R4,BLOCK            GET SET TO VALIDATE STATION SYNTAX           
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
*                                                                               
         MVC   QSTANEW,STBSTA      SET OUTPUT STATION                           
         CLI   QSTANEW+4,C' '      TEST BAND PRESENT                            
         BNE   *+10                                                             
         MVC   QSTANEW+4(1),QMED                                                
         DROP  R4                                                               
*                                                                               
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
VK10     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ADRRECD,R4                                                       
         MVI   ADDKTYPE,ADDKTYPQ   ADDRESS RECORDS ARE TYPE-'A'.                
         MVC   ADDKMED,QMED        MEDIA.                                       
         MVC   ADDKCALL,QSTANEW    CALL LETTERS.                                
         MVC   ADDKAGY,AGENCY      AGENCY.                                      
         DROP  R4                                                               
         MVC   MYSVKEY(ADDKEYLQ),KEY    SAVE THE KEY AROUND.                    
*                                                                               
XVK      BAS   RE,SF                                                            
         BAS   RE,TESTSEL          TEST SELECT FIELDS.                          
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
TESTSEL1 LA    R2,ADLSELH          R2-->FIRST SELECT FIELD.                     
         LA    RF,ADLTAGH          RF-->LAST FIELD.                             
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
         OC    KEY(ADDKEYLQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LRRDHI              NO, READ WHERE LEFT OFF.                     
*                                                                               
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
*                                                                               
LRRDHI   GOTO1 HIGH                                                             
*                                                                               
LR10     CLC   KEY(ADDKCALL-ADRRECD),MYSVKEY   IS 1ST TWO BYTES                 
         BNE   XLR                   WHAT WE'RE LOOKING FOR?                    
*                                     NO, EXIT LISTING.                         
         CLC   KEY+(ADDKAGY-ADRRECD)(L'ADDKAGY),MYSVKEY+(ADDKAGY-ADRREC+        
               D)                                                               
         BNE   LR100               AGENCY DOESN'T MATCH...NEXT RECORD.          
*                                                                               
         CLI   ADLMED,C'*'                                                      
         BNE   LR20                                                             
         CLC   KEY+(ADDKCALL-ADRRECD)(4),=C'BOX9'                               
         BH    XLR                                                              
         B     LR30                                                             
LR20     CLC   KEY+(ADDKCALL-ADRRECD)(3),=C'BOX'                                
         BE    LR100                                                            
*                                                                               
*----------------------- PUT DATA ON LIST-LINE -----------------------*         
*                                                                               
         USING ADRRECD,R4                                                       
LR30     L     R4,AIO                                                           
         USING LISTD,R2                                                         
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
*                                                                               
         MVC   LSTSTAT(L'ADDKCALL),ADDKCALL    STATION CALL LETTERS             
         CLI   ADLMED,C'*'                                                      
         BNE   *+8                                                              
         MVI   LSTSTAT+4,C' '       DON'T SHOW MEDIA                            
         MVC   LSTNAME,ANAME                   NAME                             
         MVC   LSTCITY,A2LINE                  CITY                             
         MVC   LSTSTTE,A3LINE                  STATE                            
         MVC   LSTZIP,ABIGZIP                  ZIP CODE                         
         CLC   ABIGZIP,SPACES                                                   
         BH    LR80                                                             
         CLC   AZIP,=C'CANAD'                                                   
         BNE   *+10                                                             
         MVC   LSTZIP,=C'CANAD'                                                 
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
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
         DS    0F                                                               
ZEROES   DC    (L'KEY)CL1'0'                                                    
BLANKS   DC    CL132' '                                                         
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
***********************************************************************         
         EJECT                                                                  
**********************************************************************          
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,59,REQUESTOR                                                  
         SSPEC H2,59,REPORT                                                     
         SSPEC H2,72,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'ADDRESS LIST'                                            
         SSPEC H2,30,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'STATION'                                                  
         SSPEC H4,11,C'NAME'                                                    
         SSPEC H4,33,C'CITY'                                                    
         SSPEC H4,59,C'ST'                                                      
         SSPEC H4,64,C'ZIP CODE'                                                
         SPACE 1                                                                
         SSPEC H5,1,C'-------'                                                  
         SSPEC H5,11,C'----'                                                    
         SSPEC H5,33,C'----'                                                    
         SSPEC H5,59,C'--'                                                      
         SSPEC H5,64,C'--------'                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
*====================== ADDRESS RECORDS DSECT =======================*          
ADRRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*=========================== STABLK DSECT ===========================*          
       ++INCLUDE SPSTABLK                                                       
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
       ++INCLUDE SPSFMA2D          DSECT FOR RECORD LISTING.                    
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= SPSFM WORK AREA ==========================*          
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
         ORG   SYSSPARE                                                         
**************************** WARNING ******************************             
* IF YOU WANT TO ADD ANYTHING TO SYSSPARE ADD IT IN SPSFMADR      *             
**************************** WARNING ******************************             
       ++INCLUDE SPSFMADR                                                       
         EJECT                                                                  
**********************************************************************          
*============================ MY DSECTS =============================*          
*                                                                               
*----------------------- ON-SCREEN LIST LINE ------------------------*          
*                                                                               
LISTD    DSECT                                                                  
LSTSTAT  DS    CL8                                                              
         DS    CL2                                                              
LSTNAME  DS    CL20                                                             
         DS    CL2                                                              
LSTCITY  DS    CL24                                                             
         DS    CL2                                                              
LSTSTTE  DS    CL3                                                              
         DS    CL2                                                              
LSTZIP   DS    CL10                                                             
**********************************************************************          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPSFM32   08/11/11'                                      
         END                                                                    
