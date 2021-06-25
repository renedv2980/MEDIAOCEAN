*          DATA SET TAGENAF    AT LEVEL 023 AS OF 03/24/04                      
*PHASE T702AFA                                                                  
         TITLE 'T702AF - ADJUSTMENT LIST'                                       
T702AF   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702AF                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SADPHED(28),=C'Sel Ref#   Type    $ Emp Pid'                     
         OI    SADPHEDH+6,X'80'                                                 
         CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BE    VK                                                               
         CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   INVL30                                                           
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
*                                                                               
INVL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS                                     
*                                                                               
VK       LA    R2,SADSTRH                                                       
         TM    4(R2),X'20'         IF START AT FIELD NOT PREV VALIDATED         
         BO    VK10                                                             
         XC    TIQSTART,TIQSTART                                                
         NI    SADTYPEH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0             IF THERE'S START INPUT                       
         BE    VK10                                                             
         GOTO1 TINVCON,DMCB,SADSTR,TIQSTART,DATCON  CONVERT                     
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    TIQSTART(6),ALLFF   COMPLEMENTED                                 
VK10     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SADTYPEH                                                      
         TM    4(R2),X'20'         TYPE FILTER                                  
         BO    VK20                                                             
         NI    SADCURH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         L     R3,TGADJST          R3=A(ADJUSTMENT TABLE)                       
         USING ADJTYPSD,R3                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                R1=L'INPUT-1                                 
*                                                                               
VK15     CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    FLDINV                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ADJDESC(0),8(R2)    MATCH ON DESCRIPTION                         
         BE    VK20                                                             
         LA    R3,ADJNXT(R3)       BUMP TABLE                                   
         B     VK15                                                             
*                                                                               
VK20     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SADCURH                                                       
         TM    4(R2),X'20'         CURRENCY FILTER                              
         BO    VK30                                                             
         NI    SADEMPH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVC   TIFCUR,8(R2)                                                     
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         CLI   8(R2),C'U'          TEST US$                                     
         BE    VK30                                                             
         CLI   8(R2),C'C'          TEST CAN$                                    
         BNE   FLDINV                                                           
VK30     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SADEMPH                                                       
         TM    4(R2),X'20'         EMPLOYER FILTER                              
         BO    VK40                                                             
         NI    SADAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFEMP,TIFEMP                                                    
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2)                                         
         MVC   TIFEMP,TGEMP                                                     
VK40     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SADAGYH                                                       
         TM    4(R2),X'20'         AGENCY FILTER                                
         BO    VK50                                                             
         NI    SADERRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFAGY,TIFAGY                                                    
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
         MVC   TIFAGY,TGAGY                                                     
VK50     OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,SADERRH                                                       
         TM    4(R2),X'20'         ERROR FILTER                                 
         BO    VKX                                                              
         NI    TIFINSTY,ALL-TAINSERR                                            
         NI    TIFINSTN,ALL-TAINSERR                                            
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         OI    TIFINSTY,TAINSERR                                                
         B     VK60                                                             
         CLI   8(R2),C'N'                                                       
         BNE   FLDINV                                                           
         OI    TIFINSTN,TAINSERR                                                
VK60     OI    4(R2),X'20'                                                      
*                                                                               
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLINCDQ      SET READ SEQUENCE                            
         OI    TIFINS2Y,TAINSADJ   ASK FOR ADJUSTMENTS ONLY                     
         XC    KEY,KEY                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
LSTREC   LA    R0,LRHOOK                                                        
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   HAVE GENCON RETURN XTRA TIME FOR EOP         
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(18,R1),=C'ADJUSTMENT RECORDS'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
         USING LISTD,R2                                                         
         USING TLIND,R4                                                         
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRRX                                                             
         MVC   LISTAR,SPACES                                                    
         L     R4,TIAREC                                                        
         MVC   TGINV,TLININV                                                    
         XC    TGINV,=6X'FF'       UNCOMPLEMENT FOR DISLAY                      
         GOTO1 TINVCON,DMCB,TGINV,LISREF,DATCON CONVERT INVOICE NUMBER          
*                                                                               
         MVC   LISCUR,TICUR        CURRENCY                                     
         MVC   LISEMP,TIEMP        EMPLOYER                                     
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAOIELQ      GET OLD AGY/INV ELEMNT                       
         BAS   RE,GETEL                                                         
         BNE   PRR5                                                             
         USING TAOID,R4                                                         
         MVC   LISOAGY,TAOIAGY                                                  
         OC    TAOIINV,TAOIINV                                                  
         BZ    PRR5                                                             
         GOTO1 TINVCON,DMCB,TAOIINV,LISOINV,DATCON  CVT INVOICE NUMBER          
*                                                                               
PRR5     L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         USING TAPDD,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADJOUT,DMCB,(TAPDADJS,WORK)                                      
*                                                                               
         CLI   SADTYPEH+5,0        IF THERE'S A TYPE FILTER                     
         BE    PRR10                                                            
         ZIC   R1,SADTYPEH+5       MUST MATCH FOR L'INPUT                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SADTYPE(0),WORK                                                  
         BNE   PRRX                                                             
PRR10    MVC   LISTYPE,WORK                                                     
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAINELQ      GET INVOICE DETAILS ELEMENT                  
         USING TAIND,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   PRR20                                                            
         GOTO1 DATCON,DMCB,(1,TAINCDTE),(5,LISCKDT)                             
         OC    TAINCDTE,TAINCDTE   IF THERE'S NO CHECK DATE                     
         BNZ   PRR15                                                            
         TM    TAINSTA2,TAINSFRC   AND FORCING TO LAST YEAR                     
         BZ    PRR15                                                            
         MVC   LISCKDT,=C'LASTYEAR' DISPLAY SPECIAL LITERAL                     
*                                                                               
PRR15    GOTO1 DATCON,DMCB,(1,TAINCKRN),(5,LISRNDT)                             
         EDIT  TAINTERR,(3,LISERR)                                              
*                                                                               
PRR20    MVC   FILENAME,=CL8'CHKDIR'  SET TO GET S/S AND CHECK NUMBERS          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKD,R4                                                         
         MVI   TLCKCD,TLCKCDQ      SET RECORD TYPE                              
         MVC   TLCKAGY,=C'999999'      ADJUSTMENT AGENCY                        
         MVC   TLCKINV,TGINV                      INVOICE NUMBER                
         GOTO1 HIGH                                                             
         CLC   TLCKKEY(TLCKSORT-TLCKD),KEYSAVE                                  
         BNE   PRR25                                                            
         MVC   LISSSN,TLCKSSN      DISPLAY S/S NUMBER                           
         TM    TGSYSTAT,TASYSPID                                                
         BZ    PRR22                                                            
         MVC   LISSSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TLCKSSN,LISSSN                                      
PRR22    MVC   FILENAME,=CL8'CHKFIL'                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         USING TACDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   PRR25                                                            
         MVC   LISCHK,TACDCHK      DISPLAY CHECK NUMBER                         
*                                                                               
PRR25    XC    FILENAME,FILENAME                                                
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   PRR30                                                            
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT LINES OUTPUT                           
         B     PRRX                                                             
*                                                                               
PRR30    MVC   DMDSKADD,TIDSKADD   PASS LIST ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 3                                                                
ALLFF    DC    X'FFFFFFFFFFFF'                                                  
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'ADJUSTMENT LIST'                                         
         SSPEC H2,33,C'---------------'                                         
         SPACE 1                                                                
         SSPEC H4,1,C'REF#   TYPE    $ EMP S/S NUMB'                            
         SSPEC H5,1,C'----   ----    - --- --------'                            
         SSPEC H4,32,C'CHECK#   AGENCY INV#   CHK DATE RUN DATE ERR'            
         SSPEC H5,32,C'------   ------ ----   -------- -------- ---'            
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE 1                                                                
LISTD    DSECT                                                                  
LISREF   DS    CL6                                                              
         DS    CL1                                                              
LISTYPE  DS    CL7                                                              
         DS    CL1                                                              
LISCUR   DS    CL1                                                              
         DS    CL1                                                              
LISEMP   DS    CL3                                                              
         DS    CL1                                                              
LISSSN   DS    CL9                                                              
         DS    CL1                                                              
LISCHK   DS    CL8                                                              
         DS    CL1                                                              
LISOAGY  DS    CL6                                                              
         DS    CL1                                                              
LISOINV  DS    CL6                                                              
         DS    CL1                                                              
LISCKDT  DS    CL8                                                              
         DS    CL1                                                              
LISRNDT  DS    CL8                                                              
         DS    CL1                                                              
LISERR   DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRAFD                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023TAGENAF   03/24/04'                                      
         END                                                                    
