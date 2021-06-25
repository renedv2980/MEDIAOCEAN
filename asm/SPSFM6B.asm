*          DATA SET SPSFM6B    AT LEVEL 016 AS OF 11/01/04                      
*PHASE T2176BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2176A -- STATION FIX RECORD LIST ONLY               *         
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
         CLI   MODE,LISTRECS       LIST RECORDS                                 
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
         XC    SAVEINFO,SAVEINFO   ALL STATION FIX STORAGE DATA                 
*                                                                               
         LA    R4,SVKEY                                                         
         USING STAFXRCD,R4                                                      
         MVI   STFKTYP,STFKTYQ     MOVE RECORD TYPE X'0D' TO KEY                
         MVI   STFKSBTY,STFKSBTQ   MOVE RECORD SUBTYPE X'6E' TO KEY             
*                                                                               
         MVI   USEIONUM,3          USING AIO3 FOR TEMPORARY STORAGE...          
         LA    R2,SFLMEDKH         ...FOR THE VALIDATION SUBROUTINES            
         GOTO1 VALIMED                                                          
         MVC   STFKAGMD,BAGYMD     MOVE 1 BYTE BINARY AGENCY/MEDIA CODE         
         OI    SFLMEDKH+6,X'80'                                                 
*                                                                               
         MVC   SFLMEDN,MEDNM       MOVE MEDIA NAME                              
         OI    SFLMEDNH+6,X'80'                                                 
*                                                                               
         LA    R2,SFLCLTH          CLIENT ON SCREEN                             
         CLI   5(R2),0             HAVE A CLIENT?                               
         BE    VK10                NO                                           
         OI    FILTFLAG,CLTFIL     X'80'                                        
         CLC   SFLCLT,=C'ALL'      ALL CLIENTS?                                 
         BNE   VK08                NO                                           
         MVC   BCLT,=X'FFFF'       YES                                          
         B     VK09                                                             
*                                                                               
VK08     GOTO1 VALICLT                                                          
*                                                                               
VK09     OI    6(R2),X'80'                                                      
***                                                                             
* VALIDATE OLD STATION                                                          
***                                                                             
VK10     LA    R2,SFLOSTAH         OLD STATION                                  
         CLI   5(R2),0             HAVE ONE?                                    
         BE    VK20                NO                                           
         OI    FILTFLAG,OSTAFIL    X'40'                                        
         GOTO1 VALISTA                                                          
         MVC   SAVEOSTA,BSTA       SAVE OFF THE BINARY OLD STATION              
         OI    6(R2),X'80'                                                      
***                                                                             
* VALIDATE MARKET                                                               
***                                                                             
VK20     LA    R2,SFLMKTH          MARKET                                       
         CLI   5(R2),0             HAVE ONE?                                    
         BE    VK30                NO                                           
         OI    FILTFLAG,MKTFIL     X'20'                                        
         GOTO1 VALIMKT                                                          
         MVC   SAVEMKT,BMKT        SAVE OFF THE BINARY MARKET                   
***                                                                             
* VALIDATE NEW STATION                                                          
***                                                                             
VK30     LA    R2,SFLNSTAH         NEW STATION                                  
         CLI   5(R2),0             HAVE ONE?                                    
         BE    VK40                NO                                           
         OI    FILTFLAG,NSTAFIL    X'10'                                        
         GOTO1 VALISTA                                                          
         MVC   SAVENSTA,BSTA       SAVE OFF THE BINARY NEW STATION              
         OI    6(R2),X'80'                                                      
***                                                                             
*   DON'T REALLY NEED TO SAVE OFF EVERYTHING BUT MIGHT AS WELL                  
***                                                                             
VK40     LA    R2,SFLDATEH         DATE                                         
         CLI   5(R2),0             HAVE ONE?                                    
         BE    VKX                 NO                                           
         XC    WORK(2),WORK                                                     
         MVC   WORK(1),5(R2)       INPUT LENGTH IN FIRST BYTE OF WORK           
         OI    WORK+1,PVINSGLO     SINGLE DATE ONLY IS VALID                    
         OI    WORK+1,PVINSGLS     SINGLE DATE RETURNED AS SINGLE               
         OI    WORK,X'40'          IT'S MMDD INSTEAD OF MMYY (PERVAL)           
*                                                                               
         GOTO1 PERVAL,DMCB,(WORK,SFLDATE),(WORK+1,PERVOUT)                      
         TM    4(R1),PVRCINV1      DATE ONE INVALID?                            
         BO    ERRDATE             YEP, ERROR                                   
*                                                                               
         LA    R3,PERVOUT                                                       
         USING PERVALD,R3                                                       
         TM    PVALASSM,PVALASM    ASSUMED MONTH?                               
         BNZ   ERRDATE              YEP, ERROR                                  
         MVC   SAVEDATE,PVALCSTA   SAVE THE COMPRESSED DATE                     
         GOTO1 DATCON,DMCB,(2,SAVEDATE),(5,SFLDATE)                             
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
         LA    R2,SFLLST1H         SETUP THIS LIST SO IT DOESN'T GO FOR         
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
         BNE   LRX                 NO MORE STATION FIX RECORDS TO LIST          
*                                                                               
         LA    R4,KEY                                                           
         USING STAFXRCD,R4                                                      
*                                                                               
         TM    FILTFLAG,CLTFIL     X'80' - ANY CLIENT FILTER?                   
         BZ    LR33A               NO                                           
         CLC   STFKCLI,BCLT        MATCH ON CLIENT?                             
         BNE   LR20                NO, READ SEQ                                 
*                                                                               
LR33A    TM    FILTFLAG,OSTAFIL    X'40' - ANY OLD STATION FILTER?              
         BZ    LR33B               NO                                           
         CLC   STFKOSTA,SAVEOSTA   MATCH ON OLD STATION?                        
         BNE   LR20                NO, READ SEQ                                 
*                                                                               
LR33B    TM    FILTFLAG,MKTFIL     X'20' - ANY MARKET FILTER?                   
         BZ    LR33C               NO                                           
         CLC   STFKMKT,SAVEMKT     MATCH ON MARKET?                             
         BNE   LR20                NO, READ SEQ                                 
*                                                                               
LR33C    TM    FILTFLAG,NSTAFIL    X'10' - ANY NEW STATION FILTER?              
         BZ    LR34                NO                                           
         CLC   STFKNSTA,SAVENSTA   MATCH ON NEW STATION?                        
         BNE   LR20                NO, READ SEQ                                 
*                                                                               
LR34     XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(13),KEY                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STFIDELQ     INFO - DETAILS ELEMENT (X'10')               
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   LR20                NO. WELL, IT'S BETTER THEN DEATH!            
*                                                                               
         USING STFIDELD,R6                                                      
         CLC   SAVEDATE,STFIDDAT   IS SAVEDATE EARLIER OR SAME DATE?            
         BH    LR20                 NOPE, NEXT RECORD PLZ                       
*                                                                               
         L     R4,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
***                                                                             
* LIST THE CLIENT ON THE SCREEN                                                 
***                                                                             
         CLC   STFKCLI,=X'FFFF'    ALL CLIENTS?                                 
         BNE   LR35                NO                                           
         MVC   LSCLIENT,=C'ALL'    YES, NO NEED TO READ CLT REC                 
         B     LR50                                                             
*                                                                               
LR35     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(13),KEY     SAVE OFF THE STAFIX KEY                      
         XC    KEY,KEY             NEED TO READ CLIENT RECORD TO CHECK          
         USING CLTRECD,R5          ...FOR AAN BYTE                              
         LA    R5,KEY                                                           
         MVI   CKEYTYPE,CKEYTYPQ   RECORD TYPE X'00'                            
         MVC   CKEYAM,STFKAGMD     A/M                                          
         MVC   CKEYCLT,STFKCLI     CLIENT                                       
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(3),KEY      FOUND THE RECORD?                            
         BE    LR40                YES                                          
         GOTO1 CLUNPK,DMCB,STFKCLI,LSCLIENT                                     
         B     LR50                                                             
*                                                                               
LR40     MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R5,AIO2                                                          
         GOTO1 CLUNPK,DMCB,(CPROF+6,STFKCLI),LSCLIENT                           
         DROP  R5                                                               
***                                                                             
* LIST THE OLD STATION ON THE SCREEN                                            
***                                                                             
LR50     XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),STFKOSTA                                               
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+7,LSOLDSTA                         
***                                                                             
* LIST THE MARKET ON THE SCREEN                                                 
***                                                                             
LR60     SR    R3,R3                                                            
         ICM   R3,3,STFKMKT                                                     
         CVD   R3,DUB                                                           
         UNPK  LSMKT,DUB+4(4)                                                   
         OI    LSMKT+L'LSMKT-1,X'F0'   CONVERT FROM ZONE 2 EBCDIC               
***                                                                             
* LIST THE NEW STATION ON THE SCREEN                                            
***                                                                             
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),STFKNSTA                                               
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+7,LSNEWSTA                         
*                                                                               
         CLC   STFKCLI,=X'FFFF'                                                 
         BE    LR65                THE KEY IS ALREADY MARKET FIX                
*                                                                               
         XC    KEY,KEY             KEY PREVIOUSLY HELD CLIENT REC KEY           
         MVC   KEY(13),SAVEKEY                                                  
*                                                                               
LR65     GOTO1 READ                PUT THE KEY BACK IN SEQUENCE                 
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STFIDELQ     INFO - DETAILS ELEMENT (X'10')               
         BAS   RE,GETEL                                                         
         BNE   LRNEXT                                                           
         USING STFIDELD,R6                                                      
*                                                                               
LR66     GOTO1 DATCON,DMCB,(2,STFIDDAT),(5,LSDATE)                              
         MVC   LSRQSTOR,STFIDRQR   REQUESTOR NAME                               
         CLI   STFIDLEN,16         OLD RECORD LENGTH W/O PERSON AUTH# ?         
         BE    LR70                YES                                          
         BAS   RE,GETNAME          GET NAME FROM PERSONAL AUTH NUM              
*                                                                               
LR70     GOTO1 LISTMON                                                          
         MVC   LSNAME,SPACES       CLEAR NAME(NOT WHOLE LISTAR!)                
         BAS   RE,NEXTEL           HAVE ANOTHER X'10' ELEMENT?                  
         BE    LR66                YES                                          
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
         USING STFIDELD,R6                                                      
         MVC   SAVEKEY(13),KEY     SAVE OFF THE KEY                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'            PERSONAL AUTH RECS IN CTFILE                 
         MVC   KEY+1(2),SVSECAGY                                                
         MVC   KEY+23(2),STFIDATH  PERSONAL AUTH NUMBER                         
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
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
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
BADDATE  EQU   20                  INVALID DATE FORMAT                          
*                                                                               
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
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM46D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAFX        STATION FIX RECORD DSECT                     
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD DSECT                          
         EJECT                                                                  
       ++INCLUDE FASECRETD         SECURITY ACCESS                              
         EJECT                                                                  
       ++INCLUDE SEACSFILE         SECURITY SYSTEM RECORD DSECTS                
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
*        PRINT ON                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
FILTFLAG DS    XL1                 FILTER FLAGS                                 
CLTFIL   EQU   X'80'                CLIENT FILTER      = YES                    
OSTAFIL  EQU   X'40'                OLD STATION FILTER = YES                    
MKTFIL   EQU   X'20'                MARKET FILTER      = YES                    
NSTAFIL  EQU   X'10'                NEW STATION FILTER = YES                    
*                                                                               
ERRNUM   DS    XL2                                                              
SAVEINFO DS    0CL10                                                            
SAVEOSTA DS    XL3                 SAVE THE OLD STATION (BINARY)                
SAVEMKT  DS    CL2                 SAVE THE MARKET (BINARY)                     
SAVENSTA DS    XL3                 SAVE THE NEW STATION (BINARY)                
SAVEDATE DS    XL2                 SAVE THIS DATE (COMPRESSED)                  
*                                                                               
SAVEKEY  DS    CL32                                                             
SVSECAGY DS    XL2                 SECURITY AGENCY                              
PERVOUT  DS    CL(PERVALDX-PERVALD)   OUTPUT BLOCK FOR PERVAL                   
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
         DS    CL2                                                              
LSOLDSTA DS    CL8                                                              
         DS    CL2                                                              
LSMKT    DS    CL4                                                              
         DS    CL2                                                              
LSNEWSTA DS    CL8                                                              
         DS    CL2                                                              
LSDATE   DS    CL8                                                              
         DS    CL2                                                              
LSRQSTOR DS    CL12                                                             
         DS    CL2                                                              
LSNAME   DS    CL21                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPSFM6B   11/01/04'                                      
         END                                                                    
