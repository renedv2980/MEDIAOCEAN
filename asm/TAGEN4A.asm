*          DATA SET TAGEN4A    AT LEVEL 041 AS OF 07/30/12                      
*PHASE T7024AC,*                                                                
         TITLE 'T7024A - CAGENT LIST'                                           
T7024A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7024A                                                         
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
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,0                                                   
         TM    TGSYSTAT,TASYSPID   ARE WE USING PIDS?                           
         BZ    CAGNTL10            NO                                           
         MVC   SCASSND(7),=CL7'Pid'                                             
         OI    SCASSNDH+6,X'80'                                                 
         MVC   SCATAG(11),=CL11'Sel Pid    '                                    
         OI    SCATAGH+6,X'80'                                                  
*                                                                               
CAGNTL10 CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   CAGNTL20                                                         
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         SPACE 3                                                                
CAGNTL20 CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   CAGNTL30                                                         
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
CAGNTL30 CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         SPACE 3                                                                
LSTREC   EQU   *                                                                
         LA    R0,HOOK             SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         MVI   NLISTS,9            GET CONTROL BACK AT END OF PAGE              
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   TIERROR,TINOTOLN                                                 
         BNE   LSTR3                                                            
         LA    R2,CONRECH                                                       
         B     ONLINERR            CANNOT RUN ONLINE                            
         SPACE 1                                                                
LSTR3    MVI   NLISTS,8            RESET TO 8 LIST LINES AT END OF LIST         
         CLI   MODE,PRINTREP       IF NOT PRINTING REPORT                       
         BE    LSTR5                                                            
         L     R3,ATHISLST                                                      
         LA    R4,SCALSTH                                                       
         CR    R3,R4               AND NOT AT BOTTOM OF SCREEN                  
         BNL   LSTRX                                                            
         TWAXC (R3),(R4),PROT=Y    CLEAR REST OF SCREEN                         
         B     LSTRX                                                            
         SPACE                                                                  
LSTR5    GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(12,R1),=C'CAST RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LSTRX                                                            
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE                                                                  
LSTRX    MVC   TGCOM,SVTGCOM       RESTORE GLOBAL VALUES                        
         MVC   TGCLI,SVTGCLI                                                    
         MVC   TGAGY,SVTGAGY                                                    
         MVC   TGSSN,SVTGSSN                                                    
         MVC   TGPRD,SVTGPRD                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         TM    SCAAGTH+4,X'20'     IF AGENT NOT PREV VALIDATED                  
         BO    INIT2                                                            
         MVI   ALLVAL,C'N'                                                      
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'08',SCAAGTH),SCAAGTNH  GET NAME           
         MVC   TIFAGT,SCAAGT       FILTER ON AGENT                              
         OC    TIFAGT,SPACES                                                    
         SPACE                                                                  
INIT2    TM    SCASSNH+4,X'20'     IF SS NUMBER NOT PREV VALIDATED              
         BO    INIT3                                                            
         MVI   ALLVAL,C'N'                                                      
         NI    SCAAGYH+4,X'DF'     FORCE TO VALIDATE AGENCY                     
         XC    TIFSSN,TIFSSN       CLEAR IN CASE NO INPUT                       
         XC    SCASSNN,SCASSNN                                                  
         OI    SCASSNNH+6,X'80'                                                 
*                                                                               
         CLI   SCASSNH+5,0         IF THERE'S SSN INPUT                         
         BE    INIT2Z                                                           
         TM    TGSYSTAT,TASYSPID                                                
         BZ    INIT2X                                                           
         CLI   SCASSNH+5,6                                                      
         BH    INIT2X                                                           
         GOTO1 SSNUNPK,DMCB,SCASSN,WORK                                         
         BNE   INIT2X                                                           
         MVI   SCASSNH+5,9                                                      
         MVC   SCASSN(9),WORK                                                   
*                                                                               
INIT2X   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SCASSNH),SCASSNNH                     
         MVC   TIFSSN,SCASSN       FILTER ON SS NUMBER                          
         TM    TGSYSTAT,TASYSPID                                                
         BZ    INIT2Z                                                           
         GOTO1 SSNPACK,DMCB,SCASSN,WORK                                         
         MVC   SCASSN,SPACES                                                    
         MVC   SCASSN(6),WORK                                                   
         MVI   SCASSNH+5,6                                                      
         OI    SCASSNH+6,X'80'                                                  
INIT2Z   OI    SCASSNH+4,X'20'     SET PREV VALIDATED FOR LENGTH 0              
         SPACE                                                                  
INIT3    TM    SCAAGYH+4,X'20'     IF AGENCY NOT PREV VALIDATED                 
         BO    INIT10                                                           
         MVI   ALLVAL,C'N'                                                      
         XC    TIFAGY,TIFAGY       CLEAR IN CASE NO AGENCY INPUT                
         XC    SCAAGYN,SCAAGYN                                                  
         OI    SCAAGYNH+6,X'80'                                                 
         CLI   SCAAGYH+5,0         IF THERE'S AGENCY INPUT                      
         BE    INIT3A                                                           
         LA    R2,SCASSNH                                                       
         CLI   5(R2),0                                                          
         BE    FLDMISS             ERROR IF NO SSN INPUT                        
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SCAAGYH),SCAAGYNH                     
         MVC   TIFAGY,SCAAGY       FILTER ON AGENCY                             
         OC    TIFAGY,SPACES                                                    
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
INIT3A   OI    SCAAGYH+4,X'20'     SET PREV VALIDATED FOR LENGTH=0              
         SPACE                                                                  
INIT10   CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIREAD,TLCAACDQ     LIST BY AGENT CAST                           
         SPACE                                                                  
         MVC   SVTGCOM,TGCOM       SAVE GLOBAL VALUES - XNAME CREAMS            
         MVC   SVTGSSN,TGSSN                                                    
         MVC   SVTGAGY,TGAGY                                                    
         MVC   SVTGCLI,TGCLI                                                    
         MVC   SVTGPRD,TGPRD                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BE    HK2                                                              
         SPACE                                                                  
         CLI   LISTNUM,8           IF ALREADY FILLED PAGE                       
         BNE   HK2                                                              
         MVC   TGCOM,SVTGCOM       RESTORE GLOBAL VALUES                        
         MVC   TGCLI,SVTGCLI                                                    
         MVC   TGAGY,SVTGAGY                                                    
         MVC   TGSSN,SVTGSSN                                                    
         MVC   TGPRD,SVTGPRD                                                    
         B     ENDPAGE             GET OUT                                      
         SPACE                                                                  
HK2      MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         TM    TGSYSTAT,TASYSPID                                                
         BZ    HK2X                                                             
         GOTO1 SSNPACK,DMCB,TISSN,LISSSN                                        
         B     HK2Z                                                             
HK2X     MVC   LISSSN,TISSN        SS NUMBER                                    
HK2Z     MVC   LISCAT,TICAT        CATEGORY                                     
         MVC   LISCAM,TIONOF       CAMERA                                       
         MVC   LISUNI,TIUN         UNION                                        
         MVC   LISLCL,TILOCL       LOCAL                                        
         MVC   LISYR,TIYEAR        CONTRACT YEAR                                
         SPACE                                                                  
         USING TLCAD,R3                                                         
         USING TACAD,R4                                                         
         L     R3,TIAREC                                                        
         LR    R4,R3                                                            
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    TACASTA3,TACASPPL   IGNORE PAYROLL PLUS EMPLOYEE                 
         BO    XIT                                                              
         DROP  R4                                                               
                                                                                
         MVC   TGCOM,TLCACOM       MOVE INTO GLOBAL FOR XNAME                   
         GOTO1 XNAME,DMCB,TLCOCCDQ,SVCID,TIKEY  GET COMM REC INTO AIO           
*                                                                               
         L     R1,AIO                                                           
         USING TLCOD,R1                                                         
         MVC   LISAGY,TLCOAGY      AGENCY                                       
         MVC   LISCID,SVCID        MOVE CID TO SCREEN                           
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   LISTITLE,TGNAME     MOVE TITLE TO SCREEN                         
         DROP  R1                                                               
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK12                                                             
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         B     HK15                                                             
*                                                                               
HK12     L     R2,ATHISLST         MOVE TO SCREEN W/O LISTMON                   
         ZIC   RE,0(R2)            BUMP 1 FIELD TO NEXT DETAIL LINE             
         AR    R2,RE                                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,8(R2)            BUMP PAST FIELD HEADER                       
         XC    LISDET,LISDET       CLEAR FIRST                                  
         SPACE                                                                  
HK15     CLI   SCASSNH+5,0         IF NOT FILTERING BY SSN                      
         BNE   HK16                                                             
         MVC   TGSSN,TISSN         PUT SSN INTO GLOBAL                          
         MVC   AIO,AIO2            DON'T CREAM COMMERCIAL RECORD                
         GOTO1 XNAME,DMCB,TLW4CDQ,LISSSNN,TIKEY GET W4 NAME                     
         MVC   AIO,AIO1            RESTORE AIO TO COMMERCIAL RECORD             
         SPACE                                                                  
         USING TLCOD,R3                                                         
HK16     L     R3,AIO                                                           
         MVC   TGAGY,TLCOAGY       PUT AGENCY INTO GLOBAL                       
         MVC   TGCLI,TLCOCLI       PUT CLIENT INTO GLOBAL                       
         MVC   AIO,AIO2            DON'T CREAM COMMERCIAL RECORD                
         GOTO1 XNAME,DMCB,TLCLCDQ,LISCLIN,TIKEY GET CLIENT NAME                 
         MVC   AIO,AIO1            RESTORE AIO TO COMMERCIAL RECORD             
         MVC   LISCLI,TGCLI                                                     
         SPACE                                                                  
         OC    TLCOPRD,TLCOPRD     IF THERE'S NO PRODUCT                        
         BNZ   HK20                                                             
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD  GET NAME IF ANY                 
         MVC   LISPRDN,TGNAME      SHOW PRODUCT NAME                            
         B     HK24                                                             
         SPACE                                                                  
HK20     MVC   TGPRD,TLCOPRD       PUT PRODUCT INTO GLOBAL                      
         GOTO1 XNAME,DMCB,TLPRCDQ,LISPRDN,TIKEY GET PRODUCT NAME                
         MVC   LISPRD,TGPRD        MOVE PRD TO SCREEN                           
         SPACE                                                                  
HK24     CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK25                                                             
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT RECORDS OUTPUT                         
         B     XIT                                                              
         SPACE                                                                  
HK25     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         L     R1,ATHISLST         UPDATE ATHISLST                              
         ZIC   RE,0(R1)            BUMP 2 FIELDS (PAST SELECT)                  
         AR    R1,RE                                                            
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         ST    R1,ATHISLST                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
ONLINERR MVI   ERROR,NOTONLIN                                                   
         B     THEEND                                                           
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCASELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'CAGENT LIST'                                             
         SSPEC H2,33,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,1,C'S/S NUM'                                                  
         SSPEC H4,12,C'AGENCY'                                                  
         SSPEC H4,20,C'COMM ID/CLI'                                             
         SSPEC H4,33,C'TITLE'                                                   
         SSPEC H4,51,C'CAT/PRD'                                                 
         SSPEC H4,60,C'CAM'                                                     
         SSPEC H4,65,C'UNI'                                                     
         SSPEC H4,69,C'LCL'                                                     
         SSPEC H4,74,C'YR'                                                      
         SPACE 1                                                                
         SSPEC H5,1,C'-------'                                                  
         SSPEC H5,12,C'------'                                                  
         SSPEC H5,20,C'-----------'                                             
         SSPEC H5,33,C'-----'                                                   
         SSPEC H5,51,C'-------'                                                 
         SSPEC H5,60,C'---'                                                     
         SSPEC H5,65,C'---'                                                     
         SSPEC H5,69,C'---'                                                     
         SSPEC H5,74,C'--'                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISDET   DS    0CL75                                                            
LISSSN   DS    CL9                                                              
         DS    CL2                                                              
LISAGY   DS    CL6                                                              
         DS    CL2                                                              
LISCID   DS    CL12                                                             
         DS    CL1                                                              
LISTITLE DS    CL16                                                             
         DS    CL2                                                              
LISCAT   DS    CL3                                                              
         DS    CL6                                                              
LISCAM   DS    CL3                                                              
         DS    CL2                                                              
LISUNI   DS    CL3                                                              
         DS    CL1                                                              
LISLCL   DS    CL3                                                              
         DS    CL2                                                              
LISYR    DS    CL2                                                              
         SPACE                                                                  
         ORG   LISSSN                                                           
LISSSNN  DS    CL16                                                             
         DS    CL3                                                              
LISCLI   DS    CL6                                                              
         DS    CL7                                                              
LISCLIN  DS    CL16                                                             
         DS    CL2                                                              
LISPRD   DS    CL6                                                              
         DS    CL3                                                              
LISPRDN  DS    CL16                                                             
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4AD                                                       
         SPACE                                                                  
COUNTER  DS    PL4                 COUNTER OF NUM OF RECORDS OUTPUT             
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
SVTGCOM  DS    XL4                 SAVED GLOBAL INTERNAL COMMERCIAL NUM         
SVTGSSN  DS    CL9                 SAVED GLOBAL SS NUMBER                       
SVTGAGY  DS    CL6                 SAVED GLOBAL AGENCY                          
SVTGCLI  DS    CL6                 SAVED GLOBAL CLIENT                          
SVTGPRD  DS    CL6                 SAVED GLOBAL PRODUCT                         
SVCID    DS    CL16                SAVED CID (NEED 16 BYTES FOR XNAME)          
         SPACE 3                                                                
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
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041TAGEN4A   07/30/12'                                      
         END                                                                    
