*          DATA SET SPSFM6A    AT LEVEL 004 AS OF 10/21/04                      
*PHASE T2176AA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2176A -- MARKET FIX RECORD LIST ONLY                *         
*                                                                     *         
*  COMMENTS:     LISTS MARKET FIX RECORDS                             *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM4B (LIST)                                *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2176A - MARKET FIX RECORD LIST'                                
T2176A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**176A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*        CLI   MODE,VALREC         VALIDATE RECORD                              
*        BE    VR                                                               
*        CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
*        BE    DK                                                               
*        CLI   MODE,DISPREC        DISPLAY RECORD                               
*        BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R5                                                        
         L     R5,DMCB                                                          
         MVC   SVSECAGY,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R5                                                               
*                                                                               
         MVI   FILTFLAG,0          CLEAR OUT THE FILTER FLAG BYTE               
         XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
         XC    SAVEINFO,SAVEINFO   ALL MARKET FIX STORAGE DATA                  
*                                                                               
         LA    R4,SVKEY                                                         
         USING MKTFXRCD,R4                                                      
         MVI   MKFKTYPE,MKFKTYPQ   MOVE RECORD TYPE X'0D' TO KEY                
         MVI   MKFKSBTY,MKFKSBTQ   MOVE RECORD SUBTYPE X'6D' TO KEY             
*                                                                               
         MVI   USEIONUM,3          USING AIO3 FOR TEMPORARY STORAGE...          
         LA    R2,MFLMEDKH         ...FOR THE VALIDATION SUBROUTINES            
         GOTO1 VALIMED                                                          
         MVC   MKFKAGMD,BAGYMD     MOVE 1 BYTE BINARY AGENCY/MEDIA CODE         
         OI    MFLMEDKH+6,X'80'                                                 
*                                                                               
         MVC   MFLMEDN,MEDNM       MOVE MEDIA NAME                              
         OI    MFLMEDNH+6,X'80'                                                 
*                                                                               
         LA    R2,MFLCLTH                                                       
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         OI    FILTFLAG,CLTFIL     X'80'                                        
         CLC   MFLCLT,=C'ALL'                                                   
         BNE   VK08                                                             
         MVC   BCLT,=X'FFFF'                                                    
         B     VK09                                                             
VK08     GOTO1 VALICLT                                                          
VK09     MVC   SAVECLT,BCLT        SAVE OFF THE BINARY CLIENT                   
         OI    6(R2),X'80'                                                      
*                                                                               
VK10     LA    R2,MFLOMKTH                                                      
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         OI    FILTFLAG,OMKTFIL    X'40'                                        
         GOTO1 VALIMKT                                                          
         MVC   SAVEOMKT,BMKT       SAVE OFF THE BINARY OLD MARKET               
*                                                                               
VK20     LA    R2,MFLSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         OI    FILTFLAG,STAFIL     X'20'                                        
*                                                                               
         CLI   QMED,C'C'           MEDIA C?                                     
         BNE   VK25                NO, DO REGULAR VALISTA CALL                  
*                                                                               
         LA    R3,MFLSTA+3         FIRST POSITION WHERE WE COULD HAVE /         
         CLI   0(R3),C'/'          HAVE ONE?                                    
         BE    VK21                YES                                          
         LA    R3,1(R3)            BUMP                                         
         CLI   0(R3),C'/'          HAVE ONE?                                    
         BNE   VK25                NO                                           
*                                                                               
VK21     CLI   1(R3),X'40'         SEARCHING FOR SPECIFIC SUFFIX?               
         BH    VK25                YES                                          
         MVI   0(R3),X'40'         PUT A SPACE SO NO ERROR FROM STAVAL          
         OI    FILTFLAG,ALLSUFX                                                 
*                                                                               
VK25     MVC   SVMED,QMED          SAVE MEDIA OFF                               
         CLI   QMED,C'C'           MEDIA C?                                     
         BNE   *+8                 NO                                           
         MVI   QMED,C'N'           YES - FAKE OUT VALISTA                       
         GOTO1 VALISTA                                                          
         MVC   QMED,SVMED          RESTORE MEDIA                                
         TM    FILTFLAG,ALLSUFX    SPACED OUT C'/'                              
         BZ    *+8                 NO                                           
         MVI   0(R3),C'/'                                                       
         MVC   SAVESTA,BSTA        SAVE OFF THE BINARY STATION                  
         OI    6(R2),X'80'                                                      
*                                                                               
VK30     LA    R2,MFLNMKTH                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         OI    FILTFLAG,NMKTFIL    X'10'                                        
         GOTO1 VALIMKT                                                          
         MVC   SAVENMKT,BMKT       SAVE OFF THE BINARY NEW MARKET               
*   DON'T REALLY NEED TO SAVE OFF EVERYTHING BUT MIGHT AS WELL                  
VK40     LA    R2,MFLDATEH                                                      
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         OI    FILTFLAG,DATEFIL    X'80'                                        
         XC    WORK(2),WORK                                                     
         MVC   WORK(1),5(R2)       INPUT LENGTH IN FIRST BYTE OF WORK           
         OI    WORK+1,PVINSGLO     SINGLE DATE ONLY IS VALID                    
         OI    WORK+1,PVINSGLS     SINGLE DATE RETURNED AS SINGLE               
         OI    WORK,X'40'          IT'S MMDD INSTEAD OF MMYY (PERVAL)           
*                                                                               
         GOTO1 PERVAL,DMCB,(WORK,MFLDATE),(WORK+1,PERVOUT)                      
         TM    4(R1),PVRCINV1      DATE ONE INVALID?                            
         BO    ERRDATE              YEP, ERROR                                  
*                                                                               
         LA    R3,PERVOUT                                                       
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASM    ASSUMED MONTH?                               
         BNZ   ERRDATE              YEP, ERROR                                  
         MVC   SAVEDATE,PVALCSTA   SAVE THE COMPRESSED DATE                     
         GOTO1 DATCON,DMCB,(2,SAVEDATE),(5,MFLDATE)                             
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),SVKEY                                                    
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
*   GENCON ALWAYS LOOK FOR THE FIRST UNPROTECTED BYTE ON THE SCREEN FOR         
*   ...FOR LIST, SO WE NEED TO MANUALLY SET "ATHISLST" TO THE FIRST             
*   ...PROTECTED FIELD WHEN THE SELECT FIELD IS NOT PRESENT    MHC              
*                                                                               
         LA    R2,MFLLST1H         SETUP THIS LIST SO IT DOESN'T GO FOR         
         ST    R2,ATHISLST         ...THE FIRST UNPROTECTED FIELD               
         MVI   NLISTS,14           MAXIMUM OF 14 RECORDS PER SCREEN             
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                 NAH                                         
*                                                                               
         MVC   KEY(13),SVKEY       GETTING READY TO DO READ HIGH                
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(3),SVKEY        SAME RECORD TYPE/MEDIA?                      
         BNE   LRX                 NO MORE MARKET FIX RECORDS TO LIST           
*                                                                               
*  CODE FOR ALL FILTERS BESIDES DATE                                            
*                                                                               
         LA    R4,KEY                                                           
         USING MKTFXRCD,R4                                                      
*                                                                               
         TM    FILTFLAG,CLTFIL     X'80' - ANY CLIENT FILTER?                   
         BZ    LR33A                                                            
         CLC   MKFKCLT,BCLT                                                     
         BNE   LR20                                                             
*                                                                               
LR33A    TM    FILTFLAG,OMKTFIL    X'40' - ANY OLD MARKET FILTER?               
         BZ    LR33B                                                            
         CLC   MKFKOMKT,SAVEOMKT                                                
         BNE   LR20                                                             
*                                                                               
LR33B    TM    FILTFLAG,STAFIL     X'20' - ANY STATION FILTER?                  
         BZ    LR33C                                                            
         TM    FILTFLAG,ALLSUFX    LOOKING FOR ALL SUFFIX?                      
         BZ    LR33B01             NO                                           
         CLI   MKFKSTA+2,X'B0'     CANADIAN CABLE?                              
         BL    LR33B01             NO                                           
         CLC   MKFKSTA(2),BSTA     COMPARE WITHOUT SUFFIX                       
         B     *+10                                                             
*                                                                               
LR33B01  CLC   MKFKSTA,BSTA                                                     
         BNE   LR20                                                             
*                                                                               
LR33C    TM    FILTFLAG,NMKTFIL    X'10' - ANY NEW MARKET FILTER?               
         BZ    LR34                                                             
         CLC   MKFKNMKT,SAVENMKT                                                
         BNE   LR20                                                             
*                                                                               
LR34     XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,MKFIDELQ     INFO - DETAILS ELEMENT (X'10')               
         BAS   RE,GETEL                                                         
         USING MKFIDELD,R6                                                      
         CLC   SAVEDATE,MKFIDDAT   IS SAVEDATE EARLIER OR SAME DATE?            
         BH    LR20                 NOPE, NEXT RECORD PLZ                       
         L     R4,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         CLC   MKFKCLT,=X'FFFF'                                                 
         BNE   LR35                                                             
         MVC   LSCLIENT,=C'ALL'                                                 
         B     LR50                                                             
LR35     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(13),KEY                                                  
         XC    KEY,KEY             NEED TO READ CLIENT RECORD TO CHECK          
         USING CLTRECD,R5          ...FOR AAN BYTE                              
         LA    R5,KEY                                                           
         MVI   CKEYTYPE,CKEYTYPQ   RECORD TYPE X'00'                            
         MVC   CKEYAM,MKFKAGMD                                                  
         MVC   CKEYCLT,MKFKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(3),KEY                                                   
         BE    LR40                                                             
         GOTO1 CLUNPK,DMCB,MKFKCLT,LSCLIENT                                     
         B     LR50                                                             
*                                                                               
LR40     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R5,AIO2                                                          
         GOTO1 CLUNPK,DMCB,(CPROF+6,MKFKCLT),LSCLIENT                           
         DROP  R5                                                               
*                                                                               
LR50     SR    R3,R3                                                            
         ICM   R3,3,MKFKOMKT                                                    
         CVD   R3,DUB                                                           
         UNPK  LSOLDMKT,DUB+4(4)                                                
         OI    LSOLDMKT+L'LSOLDMKT-1,X'F0'   CONVERT FROM ZONE 2 EBCDIC         
*                                                                               
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),MKFKSTA                                                
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+7,LSSTATN                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,MKFKNMKT                                                    
         CVD   R3,DUB                                                           
         UNPK  LSNEWMKT,DUB+4(4)                                                
         OI    LSNEWMKT+L'LSNEWMKT-1,X'F0'   CONVERT FROM ZONE 2 EBCDIC         
*                                                                               
         CLC   MKFKCLT,=X'FFFF'                                                 
         BE    LR60                THE KEY IS ALREADY MARKET FIX                
         XC    KEY,KEY             KEY PREVIOUSLY HELD CLIENT REC KEY           
         MVC   KEY(13),SAVEKEY                                                  
LR60     GOTO1 READ                PUT THE KEY BACK IN SEQUENCE                 
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,MKFIDELQ     INFO - DETAILS ELEMENT (X'10')               
         BAS   RE,GETEL                                                         
         USING MKFIDELD,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,MKFIDDAT),(5,LSDATE)                              
         MVC   LSRQSTOR,MKFIDRQR   REQUESTOR NAME                               
         CLI   MKFIDLEN,16         OLD RECORD LENGTH W/O PERSON AUTH# ?         
         BE    LR70                YES                                          
         BAS   RE,GETNAME          GET NAME FROM PERSONAL AUTH NUM              
***********************************************************************         
*                                                                               
*        CLI   MODE,PRINTREP       ARE WE PRINTING REPORTS?                     
*        BNE   *+12                 NAH                                         
*        BAS   RE,PR                                                            
*        B     LRNEXT                                                           
*                                                                               
LR70     GOTO1 LISTMON                                                          
         MVC   LSNAME,SPACES       CLEAR NAME(NOT WHOLE LISTAR!)                
         BAS   RE,NEXTEL                                                        
         BNE   LRNEXT                                                           
         GOTO1 DATCON,DMCB,(2,MKFIDDAT),(5,LSDATE)                              
         MVC   LSRQSTOR,MKFIDRQR   REQUESTOR NAME                               
         CLI   MKFIDLEN,16         OLD RECORD LENGTH W/O PERSON AUTH# ?         
         BE    LR70                YES                                          
         BAS   RE,GETNAME                                                       
         B     LR70                                                             
*                                                                               
LRNEXT   B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                       GET NAME                                      *         
***********************************************************************         
GETNAME  NTR1                                                                   
*                                                                               
         USING MKFIDELD,R6                                                      
         MVC   SAVEKEY(13),KEY     SAVE OFF THE KEY                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'            PERSONAL AUTH RECS IN CTFILE                 
         MVC   KEY+1(2),SVSECAGY                                                
         MVC   KEY+23(2),MKFIDATH  PERSONAL AUTH NUMBER                         
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
         L     R5,AIO                                                           
         CLC   KEY(25),0(R5)       FOUND IT?                                    
         BE    *+14                YES                                          
         MVC   LSNAME(13),=C'** UNKNOWN **'                                     
         B     GNX                                                              
         LA    R1,28(R5)           POINT TO FIRST ELEM                          
*                                                                               
GN10     CLI   0(R1),X'C3'         C3 ELEM?                                     
         BE    GN15                YUP                                          
         ZIC   R2,1(R1)            BUMP TO NEXT ELEM                            
         AR    R1,R2                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   GN10                NOPE                                         
         MVC   LSNAME(13),=C'** UNKNOWN **'                                     
         B     GNX                                                              
*                                                                               
GN15     XC    KEY,KEY                                                          
         MVI   KEY,C'F'                                                         
         MVI   KEY+1,X'04'                                                      
         MVC   KEY+13(2),SVSECAGY                                               
         MVC   KEY+15(8),2(R1)     PERSONAL ID                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO                       
         L     R5,AIO                                                           
         CLC   KEY(23),0(R5)       FOUND IT?                                    
         BE    *+14                YES                                          
         MVC   LSNAME(13),=C'** UNKNOWN **'                                     
         B     GNX                                                              
         LA    R1,28(R5)           POINT TO FIRST ELEM                          
*                                                                               
GN20     CLI   0(R1),X'C5'         C5 ELEM?                                     
         BE    GN25                YUP                                          
         ZIC   R2,1(R1)            BUMP TO NEXT ELEM                            
         AR    R1,R2                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   GN20                NOPE                                         
         MVC   LSNAME(13),=C'** UNKNOWN **'                                     
         B     GNX                                                              
*                                                                               
GN25     LA    R3,SANAMES-SANAMEL(R1)          FIRST NAME IN LIST               
         USING SANAMES,R3                                                       
         LA    R2,LSNAME                                                        
         TM    SANAMIND-SANAMEL(R1),SANAMILN   LAST NAME PRESENT?               
         BNZ   GN30                            YES                              
         TM    SANAMIND-SANAMEL(R1),SANAMIMN   MIDDLE NAME PRESENT?             
         BZ    GN26                            NO                               
         ZIC   R4,SANAMELN                     LENGTH OF MIDDLE NAME            
         LA    R3,1(R4,R3)                     BUMP TO NEXT NAME                
GN26     TM    SANAMIND-SANAMEL(R1),SANAMIFN   FIRST NAME PRESENT?              
         BNZ   GN27                            YES                              
         MVC   0(23,R2),=C'** NO NAME IN RECORD **'                             
         B     GNX                                                              
GN27     ZIC   R4,SANAMELN                     LENGTH OF FIRST NAME             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SANAME                                                   
         B     GNX                                                              
*                                                                               
GN30     TM    SANAMIND-SANAMEL(R1),SANAMIFN   FIRST NAME PRESENT?              
         BZ    GN31                            NO                               
         LA    R5,1(R3)                        SAVE FOR LATER                   
         ZIC   R4,SANAMELN                     LENGTH OF FIRST NAME             
         LA    R3,1(R4,R3)                     BUMP TO NEXT NAME IN REC         
*                                                                               
GN31     TM    SANAMIND-SANAMEL(R1),SANAMIMN   MIDDLE NAME PRESENT?             
         BZ    GN32                            NO                               
         ZIC   R4,SANAMELN                     LENGTH OF MIDDLE NAME            
         LA    R3,1(R4,R3)                     BUMP TO NEXT NAME IN REC         
*                                                                               
GN32     ZIC   R4,SANAMELN                     LENGTH OF NAME                   
         BCTR  R4,0                            FOR EX                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SANAME                  LAST NAME TO SCREEN              
         LA    R2,1(R4,R2)                     PUT FIRST NAME HERE              
         LA    R3,2(R4,R3)                     BUMP TO NEXT NAME                
         TM    SANAMIND-SANAMEL(R1),SANAMIFN   FIRST NAME PRESENT?              
         BZ    GNX                             NO                               
         MVI   0(R2),C','                                                       
         MVC   2(1,R2),0(R5)                   1ST LETTER OF 1ST NAME           
         DROP  R3                                                               
*                                                                               
GNX      MVC   KEY(13),SAVEKEY                                                  
         GOTO1 READ                            KEY BACK IN SEQUENCE             
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       PRINT RECORD                                  *         
***********************************************************************         
*&&DO                                                                           
PR       NTR1                                                                   
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         LA    R5,HDHOOK                                                        
         ST    R5,HEADHOOK                                                      
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PSTATN,LSSTATN                                                   
         MVC   PCLIENT,LSCLIENT                                                 
         MVC   PFAXNUM,LSFAXNUM                                                 
         MVC   PATTENT,LSATTENT                                                 
         MVC   PPHNNUM,LSPHNNUM                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     XIT                                                              
*&&                                                                             
***********************************************************************         
*        HEADSPECS                                                    *         
***********************************************************************         
HEDSPECS SSPEC H1,50,C'DESTINE RECORDS'                                         
         SSPEC H2,50,C'---------------'                                         
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,AGYADD                                                      
         SSPEC H1,105,PAGE                                                      
         SSPEC H2,105,REQUESTOR                                                 
         SSPEC H3,105,RUN                                                       
         SSPEC H4,1,C'   '                                                      
         SSPEC H4,2,C'MEDIA'                                                    
*                                                                               
         SSPEC H6,2,C'STATION'                                                  
         SSPEC H7,2,C'--------'                                                 
         SSPEC H6,12,C'CLT'                                                     
         SSPEC H7,12,C'---'                                                     
         SSPEC H6,17,C'FAX NUMBER'                                              
         SSPEC H7,17,C'----------------'                                        
         SSPEC H6,34,C'ATTENTION'                                               
         SSPEC H7,34,C'-------------------------'                               
         SSPEC H6,60,C'PHONE'                                                   
         SSPEC H7,60,C'----------------'                                        
*                                                                               
         DC    X'00'                                                            
***********************************************************************         
*        HEADHOOKS                                                              
***********************************************************************         
HDHOOK   NTR1                                                                   
         MVC   H4+11(L'QMED),QMED                                               
         MVC   H4+16(L'MEDNM),MEDNM                                             
*                                                                               
HDHKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRMED   MVC   ERRNUM,=AL2(BADMED)   INVALID MEDIA                              
         B     SPERREX                                                          
ERRSTATN MVC   ERRNUM,=AL2(BADSTATN)   INVALID STATION                          
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRFAXEL MVC   ERRNUM,=AL2(NOFAXEL)   REQUIRED FAX ELEMENT NOT FOUND            
         B     SPERREX                                                          
ERRNOFAX MVC   ERRNUM,=AL2(NOFAXINP)   NO FAX INPUTTED (REQUIRED)               
         B     SPERREX                                                          
ERRFAXNO MVC   ERRNUM,=AL2(FAXNONUM)   FAX FIELD NOT 100% NUMERIC               
         B     SPERREX                                                          
ERRCLTOF MVC   ERRNUM,=AL2(CLTOFLST)   HAVE BOTH CLIENT AND OFF LIST            
         B     SPERREX                                                          
ERROFF1  MVC   ERRNUM,=AL2(OFFCHAR1)   1ST CHAR MUST BE $ OR *                  
         B     SPERREX                                                          
ERROFF2  MVC   ERRNUM,=AL2(OFFCHAR2)   2ND CHAR MUST BE ALPHA NUMERIC           
         B     SPERREX                                                          
ERRNOSTA MVC   ERRNUM,=AL2(NOSTATN)   MISSING STATION FOR NON LIST              
         B     SPERREX                                                          
ERRNOCAB MVC   ERRNUM,=AL2(USECABLE)   CABLE STATION INVALID                    
         B     SPERREX                                                          
ERRDATE  MVC   ERRNUM,=AL2(BADDATE)   INVALID DATE FORMAT                       
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BADMED   EQU   13                  INVALID MEDIA                                
BADSTATN EQU   18                  INVALID STATION                              
BADDATE  EQU   20                  INVALID DATE FORMAT                          
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
NOFAXEL  EQU   932                 REQUIRED FAX ELEMENT NOT FOUND               
NOFAXINP EQU   934                 REQUIRED FAX FIELD INPUT MISSING             
FAXNONUM EQU   935                 NON NUMERIC PHONE NUMBER                     
CLTOFLST EQU   945                 CLIENT AND OFFICE LIST CAN'T COEXIST         
OFFCHAR1 EQU   946                 1ST CHAR MUST BE $ OR *                      
OFFCHAR2 EQU   947                 2ND CHAR MUST BE ALPHA-NUMERIC               
NOSTATN  EQU   948                 MISSING STATION FIELD FOR NON LIST           
USECABLE EQU   959                 CABLE STATION CURRENTLY INVALID              
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT4,NODELLST   CANNOT DELETE FROM LIST                      
         OI    GLSTSTAT,NOSELFLD   TAKE OUT THE SELECT FIELDS                   
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM4BD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKTFX        MARKET FIX RECORD DSECT                      
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD DSECT  DSECT                   
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA          STATION RECORD DSECT  DSECT                  
         EJECT                                                                  
       ++INCLUDE FASECRETD         SECURITY ACCESS                              
         EJECT                                                                  
       ++INCLUDE SEACSFILE         SECURITY SYSTEM RECORD DSECTS                
         EJECT                                                                  
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
FAKEFLD  DS    XL11                                                             
*                                                                               
FILTFLAG DS    XL1                 FILTER FLAGS                                 
CLTFIL   EQU   X'80'                CLIENT FILTER     = YES                     
OMKTFIL  EQU   X'40'                OLD MARKET FILTER = YES                     
STAFIL   EQU   X'20'                STATION FILTER    = YES                     
NMKTFIL  EQU   X'10'                NEW MARKET FILTER = YES                     
DATEFIL  EQU   X'08'                DATE FILTER       = YES                     
ALLSUFX  EQU   X'04'                SEARCH FOR ALL SUFFIX                       
*                                                                               
ERRNUM   DS    XL2                                                              
UNPKMKT  DS    XL4                                                              
SAVEINFO DS    0CL11                                                            
SAVECLT  DS    CL2                 SAVE THE CLIENT (BINARY)                     
SAVEOMKT DS    XL2                 SAVE THE OLD MARKET (BINARY)                 
SAVESTA  DS    CL3                 SAVE THE STATION (BINARY)                    
SAVENMKT DS    XL2                 SAVE THE NEW MARKET (BINARY)                 
SAVEDATE DS    XL2                 SAVE THIS DATE (COMPRESSED)                  
SAVEKEY  DS    CL32                                                             
SAVEMENU DS    CL1                 SAVE THE EDAYMENU VALUE                      
AMBYTE   DS    CL1                 BYTE TO SAVE AGENCY/MEDIA                    
SVMYPSWD DS    XL2                 AUTHORIZATION NUMBER                         
SVSECAGY DS    XL2                 SECURITY AGENCY                              
PERVOUT  DS    CL(PERVALDX-PERVALD)   OUTPUT BLOCK FOR PERVAL                   
SVMED    DS    CL1                 SAVED MEDIA                                  
*                                                                               
       ++INCLUDE DDPERVALD                                                      
PERVALDX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSCLIENT DS    CL3                                                              
         DS    CL3                                                              
LSOLDMKT DS    CL4                                                              
         DS    CL3                                                              
LSSTATN  DS    CL8                                                              
         DS    CL3                                                              
LSNEWMKT DS    CL4                                                              
         DS    CL3                                                              
LSDATE   DS    CL8                                                              
         DS    CL2                                                              
LSRQSTOR DS    CL12                                                             
         DS    CL2                                                              
LSNAME   DS    CL23                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PSTATN   DS    CL8                                                              
         DS    CL2                                                              
PCLIENT  DS    CL3                                                              
         DS    CL2                                                              
PFAXNUM  DS    CL16                                                             
         DS    CL1                                                              
PATTENT  DS    CL25                                                             
         DS    CL1                                                              
PPHNNUM  DS    CL16                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPSFM6A   10/21/04'                                      
         END                                                                    
