*          DATA SET NESFM35S   AT LEVEL 005 AS OF 05/01/02                      
*PHASE T31C35A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: NESFM35<==>T31C35 USERFIELD HEADER LISTING.                 *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE CLIENT USERFIELD HEADERS.            *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T31C00), WHICH CALLS                  *         
*               GEGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:                                                          *         
*                                                                     *         
*  INPUTS: SCREENS NESFMED (T31CED) -- LISTING                        *         
*                                                                     *         
*  OUTPUTS: A LIST OF USERFIELD HEADERS (STORED IN CLIENT RECORDS).   *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK, USED IN GETEL                                   *         
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
         TITLE 'NESFM36<==>T31C35 USERFIELD HEADER LISTING'                     
***********************************************************************         
T31C35   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T31C35*,R7,RR=R3                                              
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
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS.                                
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
*                                                                               
*--------------------------- MEDIA FIELD -----------------------------*         
*                                                                               
         LA    R2,USLMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
*-------------------------- CLIENT FIELD -----------------------------*         
*                                                                               
         MVC   CLNT,=C'AAA'        ASSUME NO CLIENT INPUT.                      
         LA    R2,USLCLNTH                                                      
         CLI   5(R2),0                                                          
         BE    VK20                THERE IS NO INPUT.                           
*                                                                               
         LA    R0,3                                                             
         LA    R1,CLNT             GOING TO FILL IN ANY NON-                    
VK15     LA    R3,ALPHANUM          ALPHABETIC INPUT WITH C'A'.                 
VK15A    CLI   0(R3),C'/'          LETTER OF INPUT ISN'T ALPHANUMERIC,          
         BE    VK15C                LEAVE IT ALONE.                             
         CLC   8(1,R2),0(R3)       IS LETTER OF INPUT ALPHANUMERIC?             
         BE    VK15B                YES, MOVE THAT LETTER INTO CLNT.            
         LA    R3,1(R3)             NO, CHECK AGAINST NEXT ALPHANUM.            
         B     VK15A                                                            
VK15B    MVC   0(1,R1),8(R2)                                                    
VK15C    LA    R1,1(R1)            BUMP TO NEXT CHAR IN CLNT.                   
         LA    R2,1(R2)            BUMP TO NEXT CHAR IN INPUT.                  
         BCT   R0,VK15                                                          
*                                                                               
VK20     GOTO1 CLPACK,DMCB,CLNT,MYBCLT                                          
         CLI   0(R1),0                                                          
         BE    *+6                 I'VE CHECKED TO MAKE SURE THAT               
         DC    H'0'                 CLNT IS SYNTACTICALLY CORRECT.              
*                                                                               
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
VK10     BAS   RE,DOMEDTBL                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         MVI   CKEYTYPE,X'00'      CLT RECORDS ARE TYPE-X'00'.                  
         MVC   CKEYAM,BAGYMD       AGY/MEDIA.                                   
         MVC   CKEYCLT,MYBCLT      BINARY CLIENT.                               
         DROP  R4                                                               
*                                                                               
XVK      MVC   MYSVKEY,KEY                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*======================== LIST RECORDS ROUTINE =======================*         
*                                                                               
LR       DS    0H                                                               
*                                                                               
*------------------------ ADMINISTRATIVE WORK ------------------------*         
*                                                                               
         OC    KEY(L'CKEY),KEY     FIRST TIME THROUGH?                          
         BNZ   LRRDHI               NO, READ WHERE LEFT OFF.                    
*                                                                               
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
*                                                                               
LRRDHI   GOTO1 HIGH                                                             
*                                                                               
LR10     CLC   KEY(2),MYSVKEY   IS 1ST TWO BYTES                                
         BNE   XLR               WHAT WE'RE LOOKING FOR?                        
*                                  NO, EXIT LISTING.                            
*                                                                               
*----------------------- PUT DATA ON LIST-LINE -----------------------*         
*                                                                               
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
*                                                                               
         XCEF  (R4),2000                                                        
         GOTO1 GETREC                                                           
         OC    CPU1(4*CUSERLNQ),CPU1   SEE IF THERE ARE ANY USERFIELD           
         BZ    LR110                    DEFINITION DATA AT ALL.                 
*                                                                               
         MVC   LISTAR,BLANKS                                                    
         LA    R3,MEDTABLE                                                      
LR50     CLC   CKEYAM,0(R3)        MATCHING ON AGY/MED.                         
         BE    LR60                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   LR50                                                             
*                                                                               
LR60     MVC   LSTMEDI,1(R3)                   MEDIA.                           
         GOTO1 CLUNPK,DMCB,CKEYCLT,LSTCLNT     CLIENT.                          
*                                                                               
         OC    CPU1,CPU1                                                        
         BZ    LR70                                                             
         MVC   LSTDSC1,CPU1                                                     
*                                                                               
LR70     OC    CPU2,CPU2                                                        
         BZ    LR80                                                             
         MVC   LSTDSC2,CPU2                                                     
*                                                                               
LR80     OC    CEU1,CEU1                                                        
         BZ    LR90                                                             
         MVC   LSTDSC3,CEU1                                                     
*                                                                               
LR90     OC    CEU2,CEU2                                                        
         BZ    LR100                                                            
         MVC   LSTDSC4,CEU2                                                     
*                                                                               
LR100    GOTO1 LISTMON                                                          
*                                                                               
LR110    GOTO1 CATCHIOS                                                         
         GOTO1 SEQ                                                              
         B     LR10                                                             
*                                                                               
XLR      B     EXIT                                                             
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
**********************************************************************          
*============================= DOMEDTBL =============================*          
DOMEDTBL NTR1                                                                   
*         BUILDS MEDTABLE                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    MEDTABLE,MEDTABLE   CLEAR PREVIOUS ENTRIES.                      
         MVI   ELCODE,X'02'                                                     
         LA    R4,MEDTABLE                                                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DOMED10  BAS   RE,NEXTEL                                                        
         BNE   XDOMED                                                           
         CLC   2(1,R6),QMED        MATCH ON MEDIAS.                             
         BNE   DOMED10                                                          
         MVC   0(1,R4),3(R6)       BAGYMED.                                     
         MVC   1(1,R4),2(R6)       MEDIA.                                       
         LA    R4,2(R4)                                                         
         B     DOMED10                                                          
*                                                                               
XDOMED   B     EXIT                                                             
**********************************************************************          
         EJECT                                                                  
***********************************************************************         
*=============================== GETEL ===============================*         
         GETEL R6,DATADISP,ELCODE                                               
**********************************************************************          
         SPACE 4                                                                
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
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/'                         
BLANKS   DC    CL132' '                                                         
***********************************************************************         
         SPACE 4                                                                
**********************************************************************          
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*======================== CLT RECORDS DSECT =========================*          
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*======================== AGY RECORDS DSECT =========================*          
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
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
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMEDD          DSECT FOR RECORD LISTING.                    
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= NESFM WORK AREA ==========================*          
       ++INCLUDE NESFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
** STORAGE USED BY NESFM34 AS WELL                                              
*                                                                               
MEDTABLE DS    6CL2                       MEDTABLE                              
*                                     AGY/MED   MEDIA                           
*                                    |--------º-------|                         
*                                    |   +0   º  +1   |                         
*                                    |--------º-------|                         
*                                    |   +2   º  +3   |                         
*                                    :        :       :                         
*                                    :        :       :                         
*                                    |   +11  º  +12  |                         
*                                    |--------º-------|                         
*                                                                               
** CAN PUT ANY STORAGE HERE                                                     
*                                                                               
MYSVKEY  DS    CL(L'KEY)                                                        
*                                                                               
CLNT     DS    CL3                 EBCDIC CLIENT.                               
MYBCLT   DS    XL2                 CLPACKED CLIENT.                             
MYLEN    DS    CL2                 EBCDIC LENGTH.                               
RELO     DS    F                   RELOCATION FACTOR.                           
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*============================ MY DSECTS =============================*          
*                                                                               
*----------------------- ON-SCREEN LIST LINE ------------------------*          
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMEDI  DS    CL1                 MEDIA.                                       
         DS    CL1                                                              
LSTCLNT  DS    CL3                 CLIENT.                                      
         DS    CL1                                                              
LSTDSC1  DS    CL16                PRODUCT1 DESCRIPTION.                        
         DS    CL2                                                              
LSTDSC2  DS    CL16                PRODUCT2 DESCRIPTION.                        
         DS    CL2                                                              
LSTDSC3  DS    CL16                ESTIMATE1 DESCRIPTION.                       
         DS    CL2                                                              
LSTDSC4  DS    CL15                ESTIMATE2 DESCRIPTION.                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NESFM35S  05/01/02'                                      
         END                                                                    
