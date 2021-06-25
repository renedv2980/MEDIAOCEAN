*          DATA SET TAREP2C    AT LEVEL 113 AS OF 05/09/15                      
*PHASE T7032CC                                                                  
*INCLUDE TALIM                                                                  
         TITLE 'T7032C - W2 PRINT/REPRINT'                                      
T7032C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032C,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         BAS   RE,MYCLEAR                                                       
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         DROP  R1                                                               
         SPACE 1                                                                
         L     R1,TWAMASTC         SET RERUN STATUS                             
         USING MASTD,R1                                                         
         MVC   MYRERUN,MCRERUN                                                  
         MVC   MYAREMOT,MCVREMOT   SAVE A(REMOTE)                               
         DROP  R1                                                               
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         OC    ABUFFER,ABUFFER     FREEMAIN IF NEEDED                           
         BZ    XIT                                                              
         L     R0,LBUFFER                                                       
         L     R1,ABUFFER                                                       
         FREEMAIN RC,A=(1),LV=(0)                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 2                                                                
VREC     NTR1                                                                   
         MVI   TAPEOPT,C'N'                                                     
         MVI   ALLOPT,C'N'                                                      
         SPACE 1                                                                
         LA    R2,SPLYEARH         YEAR                                         
         GOTO1 ANY                                                              
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   INVXIT                                                           
         MVC   YEAR(4),8(R2)                                                    
         MVC   TIFYEAR(2),10(R2)                                                
         MVI   TIFYEAR+2,C' '      PAD WITH SPACE                               
         SPACE 1                                                                
         LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC2                                                            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2)                                         
         MVC   TIFEMP,TGEMP                                                     
         SPACE 1                                                                
VREC2    LA    R2,SPLUNTH          UNIT                                         
         XC    FILTCITY,FILTCITY                                                
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   INVXIT                                                           
         MVC   TIFUNIT,WORK                                                     
         CLI   TIFUNIT+2,C' '      IF INPUT IS CITY                             
         BNH   VREC4                                                            
         MVC   FILTCITY,TIFUNIT    MOVE IT TO CITY FILTER                       
         MVC   TIFUNIT,TGTASTCY    AND SET CORRES. STATE FILTER                 
         SPACE 1                                                                
VREC4    LA    R2,SPLSSNH          SS#                                          
         TM    WHEN,X'20'          IF SOON                                      
         BO    *+12                FIELD REQUIRED                               
         CLI   5(R2),0             ELSE, OPTIONAL                               
         BE    VREC6                                                            
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)                                         
         MVC   TIFSSN,TGSSN                                                     
         SPACE 1                                                                
VREC6    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   VRECX                                                            
         L     R1,MYAREMOT         R1=A(REMOTE)                                 
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   IF GOING DIRECT                              
         BZ    VRECX                                                            
         MVC   REMOTKEY(8),=C'W2PRINT-'  SHOW YEAR IN REPORT NAME               
         MVC   REMOTKEY+8(2),TIFYEAR                                            
VRECX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VOPTS    NTR1                                                                   
         MVI   WRITOPT,C'Y'                                                     
         MVI   TOTOPT,C'N'                                                      
         MVI   SKIPOPT,C'N'                                                     
         MVI   CDOPT,C'N'          BURNING A CD                                 
         MVI   NYPHLOPT,C'N'                                                    
         XC    SSNSTART,SSNSTART                                                
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVXIT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT3                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT3     CLC   12(5,R4),=C'WRITE'  WRITE OPTION                                 
         BNE   OPT4                                                             
         MVI   WRITOPT,C'N'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    INVXIT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    INVXIT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(3,R4),=C'ALL'    OPTION TO PRINT ALL                          
         BNE   OPT8                                                             
         MVI   ALLOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(6,R4),=C'TOTALS'  OPTION TO PRINT TOTALS                      
         BNE   OPT10                                                            
         CLI   RECNUM,WS           ONLY VALID FOR W2SPOOL                       
         BNE   INVXIT                                                           
         LA    R2,CONOUTH          AND MUST OVERRIDE OUTPUT TO PRINT            
         GOTO1 ANY                                                              
         MVI   TOTOPT,C'Y'         SET TOTALS ONLY                              
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'SKIP'   SKIP TO NEXT PAGE TO PRINT EACH W2           
         BNE   OPT15               ON A SEPARATE PAGE                           
         MVI   SKIPOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT15    CLC   12(3,R4),=C'SSN'    OPTION TO START WITH THIS SSN                
         BNE   OPT20               AND PRINT 10000 W2 RECORDS                   
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    INVXIT                                                           
         MVC   SSNSTART,22(R4)                                                  
         LA    RE,22(R4)                                                        
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'80',(RE))                                 
         MVC   TIFSSN,TGSSN                                                     
         MVC   TIQSTART(L'TIFSSN),TIFSSN                                        
*        CP    REPLIMIT,=PL6'0'    IF REPLIMIT IS SET DON'T CHANGE              
*        BNE   *+10                                                             
*        MVC   REPLIMIT,=PL6'10000' PRINT 10000 W2'S                            
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(2,R4),=C'CD'     OPTION TO BURN CD, SORT BY SSN/STATE         
         BNE   OPT25                                                            
         MVI   CDOPT,C'Y'                                                       
         B     OPTEND                                                           
                                                                                
OPT25    CLC   12(6,R4),=C'NYCPHL'   OPTION TO JUST PRINT                       
         BNE   OPT30                    WITH NO STATES                          
         MVI   NYPHLOPT,C'Y'                                                    
         B     OPTEND                                                           
OPT30    DS    0H                                                               
         B     INVXIT                                                           
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
INVXIT   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'0'                                                           
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
*                                                                               
         MVC   MYSORTER,SORTER                                                  
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(8),=C'W2 PRINT'                                          
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         OI    TIQFLAGS,TIQFDIR    WE'RE FILTERING DIRECTORY                    
         CLI   RECNUM,WS           IF GENERATING SPOOL TAPE                     
         BE    PREP20                                                           
         CLI   CDOPT,C'Y'          OR BURNING A CD                              
         BE    PREP20                                                           
         TM    WHEN,X'20'          OR IF SOON PROCESSING                        
         BZ    PREP50                                                           
PREP20   MVI   WRITOPT,C'N'        DON'T WRITE TO FILE                          
         MVI   ALLOPT,C'Y'         DON'T CHECK PENDING PRINT STATUS             
         MVC   TIQSTART(L'TIFSSN),TIFSSN AND SET SSN                            
         B     *+8                                                              
PREP50   OI    TIQFLAGS,TIQFUPRC   FOR OV, READ FOR UPDATE                      
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,DOREST           SORT, PRINT OR WRITE TAPE                    
*                                                                               
         CLI   TOTOPT,C'Y'         IF ASKED FOR TOTALS                          
         BE    *+8                                                              
         CLI   CDOPT,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTOT          PRINT THEM                                   
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOKDR                                                         
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         CLC   TISSN,SSNSTART      SKIP IF SSN LOWER THAN START SSN             
         BL    XIT                                                              
         SPACE 1                                                                
         AP    W2COUNT,=P'1'                                                    
         CP    W2COUNT,TRALIMIT                                                 
         BH    IOHOOK2                                                          
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
IOHOOK2  BAS   RE,PROCW2           PROCESS A W2 RECORD                          
         B     XIT                                                              
         SPACE 1                                                                
*                                  CHECK DIRECTORY                              
IOHOOKDR CLC   TISSN,LASTSSN       ONLY INTERESTED IN FIRST FOR SS#             
         BE    NOGOOD                                                           
         MVC   LASTSSN,TISSN                                                    
         CLI   ALLOPT,C'Y'         OK IF PROCESSING ALL                         
         BE    ITSFINE                                                          
         CLI   MYRERUN,C'Y'        OR,IF IT'S A RERUN                           
         BE    ITSFINE                                                          
         TM    TIKEYST,X'40'       OR, IF PRINT PENDING                         
         BO    ITSFINE                                                          
         B     NOGOOD                                                           
         EJECT                                                                  
*              PROCESS A W2 RECORD                                              
         SPACE 3                                                                
PROCW2   NTR1                                                                   
         XC    SORTIO(SORTLNQ),SORTIO CLEAR SORT RECORD                         
         MVI   MYBYTE,0                                                         
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAWSELQ      LOOK FOR PRINT DATE                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAWSD,R6                                                         
         MVI   SORTPRNT,C'O'       SET TO ORIGINAL                              
         OC    TAWSDPRT,TAWSDPRT                                                
         BZ    PROCW21                                                          
         OC    TIFUNIT,TIFUNIT                                                  
         BNZ   PROCW21                                                          
         MVI   SORTPRNT,C'D'           OR DUPLICATE                             
PROCW21  TM    TLW2STAT,TLW2SPND   OK IF PRINT PENDING                          
         BO    PROCW22                                                          
         CLI   ALLOPT,C'Y'         OR IF ALL OPTION                             
         BE    PROCW22                                                          
         CLI   MYRERUN,C'Y'        OR IF WE ARE RERUNNING                       
         BNE   XIT                                                              
         CLC   TAWSDPRT,TGTODAY1   AND IT'S PRINTED TODAY                       
         BNE   XIT                                                              
         SPACE 1                                                                
PROCW22  MVC   SORTKEMP,TIEMP                                                   
         CLI   RECNUM,WS           IF W2SPOOL                                   
         BE    *+8                                                              
         CLI   CDOPT,C'Y'          OR CD?                                       
         BNE   PROCW22F                                                         
         MVC   SORTK2SN,TISSN      SORT BY SSN FIRST                            
         B     *+10                                                             
PROCW22F MVC   SORTKSSN,TISSN                                                   
         MVC   SORTSSN,TISSN                                                    
         BAS   RE,GETFED           GET FEDERAL DETAILS                          
*        BAS   RE,GETNHAEW         GET NEW HIRE ACT EXEMPT WAGES                
         SPACE 1                                                                
         BRAS  RE,BUILDSTA         BUILD A LIST OF STATES                       
         BRAS  RE,ADDLOCAL         ADD LOCALS TO LIST                           
         LA    R2,UNITLIST                                                      
         CLI   0(R2),0             IF NO STATES FOUND                           
         BNE   PROCW24                                                          
         CLI   TIFUNIT,0           AND IF FILTERING ON A UNIT                   
         BE    *+14                                                             
         CLC   TIFUNIT,=C'FD '     AND UNIT IS NOT FEDERAL                      
         BNE   XIT                 THEN IGNORE                                  
         OC    SORTFERN(SORTNAMT*4),SORTFERN                                    
         BZ    XIT                 OR IF ALSO NO FEDERAL AMOUNTS                
         BAS   RE,SORTPUT          ELSE WRITE A RECORD FOR FED ONLY             
         BAS   RE,WRITEIT          WRITE BACK RECORD                            
         B     XIT                                                              
         SPACE 1                                                                
PROCW24  DS    0H                                                               
         OC    0(6,R2),0(R2)       NO MORE UNITS                                
         BZ    XIT                                                              
         OC    FILTCITY,FILTCITY   IF CITY FILTER DEFINED                       
         BZ    *+14                                                             
         CLC   FILTCITY,3(R2)      SKIP ENTRY IF NOT FOR THAT CITY              
         BNE   PROCW28                                                          
         MVC   THISUNIT,0(R2)                                                   
         MVC   SORTSTAT,THISUNIT                                                
         BAS   RE,GETSTATE                                                      
         MVC   THISUNIT,3(R2)                                                   
         MVC   SORTLOCL,THISUNIT                                                
         BAS   RE,GETLOCAL                                                      
         MVC   SORTKEMP,TIEMP                                                   
         CLI   RECNUM,WS           IF W2SPOOL                                   
         BE    *+8                                                              
         CLI   CDOPT,C'Y'                                                       
         BNE   PROCW26                                                          
         MVC   SORTK2SN,TISSN      SORT BY SSN FIRST                            
         MVC   SORTK2UN,SORTSTAT           THEN STATE                           
         B     *+16                                                             
PROCW26  MVC   SORTKSSN,TISSN      ELSE, SORT BY SSN                            
         MVC   SORTKUNT,SORTSTAT              THEN STATE                        
         BAS   RE,SORTPUT                                                       
         BAS   RE,WRITEIT          WRITE BACK RECORD IF NECESSARY               
         SPACE                                                                  
         XC    SORTFERN,SORTFERN   CLEAR OUT THE FED FIELDS                     
         XC    SORTFTAX,SORTFTAX                                                
         XC    SORTFICA,SORTFICA                                                
         XC    SORTREXP,SORTREXP                                                
         XC    SORTTXRE,SORTTXRE                                                
PROCW28  LA    R2,6(R2)                                                         
         B     PROCW24                                                          
         EJECT                                                                  
*              WRITE BACK RECORD                                                
         SPACE                                                                  
WRITEIT  NTR1                                                                   
         CLI   WRITOPT,C'Y'        TEST WRITE OPTION IS ON                      
         BNE   XIT                                                              
         TM    MYBYTE,X'80'        IF WROTE BACK RECORD ALREADY                 
         BO    XIT                 DON'T BOTHER                                 
         SPACE                                                                  
         NI    MYBYTE,X'FF'-X'40'      INIT PRINT PENDING BIT                   
         MVC   TAWSDPRT,TGTODAY1       MARK WITH TODAY'S DATE                   
         TM    TLW2STAT,TLW2SPND       IF PRINT PENDING                         
         BZ    WRITE5                                                           
         OI    MYBYTE,X'40'            SET PRINT PENDING BIT                    
         NI    TLW2STAT,X'FF'-TLW2SPND TURN OFF PRINT PENDING                   
WRITE5   MVI   TIMODE,PROCPTRC         TELL SYSIO TO WRITE BACK                 
         SPACE 1                                                                
         TM    MYBYTE,X'40'            IF PRINT PENDING                         
         BZ    WRITEX                                                           
         MVC   KEY,TIKEY                                                        
         MVC   FILENAME,=C'CHKDIR  '   GET DIRECTORY POINTER                    
         GOTO1 READ                                                             
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         NI    TLDRSTAT,X'FF'-TLW2SPND TURN OFF PRINT PENDING                   
         GOTO1 WRITE                   AND WRITE THIS BACK                      
         XC    FILENAME,FILENAME                                                
         DROP  R4                                                               
WRITEX   OI    MYBYTE,X'80'        SET WROTE BACK RECORD                        
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - DIG OUT FED AMOUNTS                        
         SPACE 3                                                                
GETFED   NTR1                                                                   
         XC    SORTFERN,SORTFERN                                                
         XC    SORTFTAX,SORTFTAX                                                
         XC    SORTFICA,SORTFICA                                                
         XC    SORTREXP,SORTREXP                                                
         XC    SORTTXRE,SORTTXRE                                                
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GETFED2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'FD '                                                 
         BNE   GETFED2                                                          
         MVC   SORTFERN,TAW2EARN   PUT IN FED EARNINGS                          
         MVC   SORTFTAX,TAW2TAX    AND TAX                                      
         MVC   SORTFICA,TAW2FICA   AND FICA                                     
         MVC   SORTREXP,TAW2REXP   AND REIMBURSED EXPENSES                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    XIT                                                              
         MVC   SORTTXRE,TAW2TXRE   AND TAXABLE REIMBURSMENTS                    
         B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
*              SUBSIDIARY ROUTINES - DIG OUT NEW HIRE ACT EXEMPT WAGES          
         SPACE 3                                                                
GETNHAEW NTR1                                                                   
         XC    SORTNHA,SORTNHA                                                  
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAWSD,R6                                                         
         MVC   SORTNHA,TAWSNHA     PUT IN NEW HIRE ACT EXEMPT WAGES             
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - DIG OUT STATE & LOCAL                      
         SPACE 3                                                                
GETSTATE NTR1                                                                   
         XC    SORTSERN,SORTSERN                                                
         XC    SORTSTAX,SORTSTAX                                                
         XC    SORTSDI,SORTSDI                                                  
         XC    SORTSUI,SORTSUI                                                  
         XC    SORTSTXR,SORTSTXR                                                
         CLI   THISUNIT,0                                                       
         BE    XIT                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GETSTA2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,THISUNIT                                                
         BNE   GETSTA2                                                          
         MVC   SORTSERN,TAW2EARN   PUT IN STATE EARNINGS                        
         MVC   SORTSTAX,TAW2TAX    AND TAX                                      
         MVC   SORTSDI,TAW2SDI     AND SDI                                      
         MVC   SORTSUI,TAW2SUI     AND SUI                                      
         MVC   SORTSFLI,TAW2SFLI   AND FLI                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    XIT                                                              
         MVC   SORTSTXR,TAW2TXRE                                                
         B     XIT                                                              
         SPACE 1                                                                
GETLOCAL NTR1                                                                   
         XC    SORTLERN,SORTLERN                                                
         XC    SORTLTAX,SORTLTAX                                                
         XC    SORTLTXR,SORTLTXR                                                
         CLI   THISUNIT,0                                                       
         BE    XIT                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GETLOC2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,THISUNIT                                                
         BNE   GETLOC2                                                          
         CLC   =C'LAX',TAW2UNIT    IGNORE IF LAX (FAKE CA CITY)                 
         BE    GETLOC2                                                          
         MVC   SORTLERN,TAW2EARN   PUT IN LOCAL EARNINGS                        
         MVC   SORTLTAX,TAW2TAX    AND TAX                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    XIT                                                              
         MVC   SORTLTXR,TAW2TXRE                                                
         B     XIT                                                              
         EJECT                                                                  
*              NOW HANDLE THE OUTPUT                                            
*                                                                               
DOREST   NTR1                                                                   
         XC    LASTSSN,LASTSSN                                                  
         XC    LASTSTAT,LASTSTAT                                                
                                                                                
         L     R5,ASPOOLD          ALWAYS PRINT LINE UP PAGE                    
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         DROP  R5                                                               
         BAS   RE,SPLAT                                                         
         CLI   TOTOPT,C'Y'         IF ASKED FOR TOTALS                          
         BE    *+8                 SKIP LINE-UP                                 
         BAS   RE,LINEUP                                                        
         SPACE 1                                                                
         CLI   SORTFRST,C'Y'       IF NOTHING TO GET FROM SORT                  
         BE    XIT                 XIT NOW                                      
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    TAPEKEY,TAPEKEY                                                  
DOREST2  GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   DOREST4                                                          
         B     XIT                                                              
         SPACE 1                                                                
DOREST4  MVC   SORTIO(SORTLNQ),0(R2)                                            
         CLC   LASTSSN,SORTSSN                                                  
         BE    DOREST4A                                                         
         MVC   LASTSSN,SORTSSN                                                  
         XC    LASTSTAT,LASTSTAT                                                
                                                                                
DOREST4A CLI   NYPHLOPT,C'Y'       IF SPECIAL NYC/PHL?                          
         BNE   DOREST5                                                          
         OC    SORTSTAT,SORTSTAT   NO STATE                                     
         BNZ   DOREST2                                                          
                                                                                
DOREST5  BAS   RE,NEEDEM           GET EMPLOYER & EMTAX RECORD                  
         BAS   RE,NEEDW4           DITTO WITH W4                                
         MVC   ATHISW4,NEEDAREC                                                 
         MVC   W4NAME,NEEDNAME                                                  
         MVC   W4ADD,NEEDADD                                                    
         L     R6,ATHISW4                                                       
         MVI   ELCODE,TAW4ELQ                                                   
         MVI   W4TYPE,0                                                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TAW4D,R6                                                         
         MVC   W4TYPE,TAW4TYPE                                                  
         SPACE 1                                                                
         CLI   W4TYPE,TAW4TYFO     NOR FOR FOREIGNERS                           
         BE    DOREST2                                                          
         SPACE 1                                                                
         BAS   RE,GETFICA          GET FICA INFO                                
         BAS   RE,NEGCHECK                                                      
         BNE   DOREST6                                                          
         AP    NEGCOUNT,=P'1'      SKIP IF ANY NEGATIVE AMOUNTS                 
         BAS   RE,ADDNEG           ADD SORTSSN TO NEGATIVE TABLE                
         B     DOREST2                                                          
         SPACE 1                                                                
DOREST6  OC    SORTFICA,SORTFICA   ALLOW IF ANY FICA                            
         BNZ   DOREST6A                                                         
         CLI   W4TYPE,TAW4TYCO     REJECT CORPS                                 
         BE    DOREST2                                                          
         CLI   W4TYPE,TAW4TYCA     AND CANADIANS                                
         BE    DOREST2                                                          
         CLI   W4TYPE,TAW4TYTR     AND TRUSTEES THAT HAVE NO FICA               
         BE    DOREST2                                                          
*                                                                               
DOREST6A AP    W2RCOUNT,=P'1'      ADD TO W2 REPORT COUNT                       
         CP    REPLIMIT,=P'0'      IF REPORT LIMIT                              
         BE    *+14                                                             
         CP    W2RCOUNT,REPLIMIT   ONLY REPORT ON THAT MANY W2S                 
         BH    XIT                                                              
         SPACE 1                                                                
         CLI   TOTOPT,C'Y'         IF ASKED FOR TOTALS                          
         BNE   *+12                                                             
         BAS   RE,ADDTOT           ADD TO TOTAL                                 
         B     DOREST2                                                          
         SPACE 1                                                                
         CLI   RECNUM,WS           IF GENERATING SPOOL TAPE                     
         BNE   DOREST8                                                          
         SPACE 1                                                                
         LA    R0,4                PROCESS FOUR FORMS TO ONE PAGE               
DOREST7  BAS   RE,SPLPRINT         USE ITS FORM                                 
         BAS   RE,DOSPACE          TAKE CARE OF SPACING BETWEEN FORMS           
         BCT   R0,DOREST7                                                       
         B     DOREST2             PROCESS NEXT W2                              
*                                                                               
DOREST8  CLC   YEAR,=C'1992'       1992 AND PRIOR W2 RECORDS                    
         BH    *+12                                                             
         BAS   RE,PRINTW2          USE OLD STYLE FORMS                          
         B     DOREST2                                                          
*                                                                               
         BAS   RE,NEWPRINT         ELSE, USE NEW STYLE FORMS                    
         B     DOREST2                                                          
         EJECT                                                                  
*              CHECK AMOUNTS FOR NEGATIVE                                       
*                                  XIT - SETS CC CODE                           
         SPACE 1                                                                
NEGCHECK NTR1                                                                   
         LA    R0,SORTNAMT         R1=NUMBER OF SORT AMOUNTS                    
         LA    RE,SORTFERN         FIRST AMOUNT TO CHECK                        
*                                                                               
NEGCHK5  L     R1,0(RE)                                                         
         LTR   R1,R1                                                            
         BM    YES                 AMOUNT NEGATIVE                              
         LA    RE,L'SORTFERN(RE)                                                
         BCT   R0,NEGCHK5                                                       
*                                                                               
         L     R1,FICAWAGE                                                      
         LTR   R1,R1                                                            
         BM    YES                                                              
         L     R1,MEDWAGE                                                       
         LTR   R1,R1                                                            
         BM    YES                                                              
         L     R1,MEDTAX                                                        
         LTR   R1,R1                                                            
         BM    YES                                                              
         B     NO                  ALL AMOUNTS OKAY                             
         SPACE 2                                                                
*              ROUTINE TO ADD SSN TO TABLE                                      
         SPACE 1                                                                
ADDNEG   NTR1                                                                   
         CLI   TOTOPT,C'Y'         IF ASKING FOR TOTALS                         
         BNE   XIT                                                              
         LA    R0,MAXNEG           ADD SSN W/NEGATIVE AMOUNTS TO TABLE          
         LA    RE,NEGTAB                                                        
ADDNEG5  OC    0(9,RE),0(RE)                                                    
         BZ    ADDNEG8                                                          
         LA    RE,9(RE)                                                         
         BCT   R0,ADDNEG5                                                       
         B     *+10                NO MORE ROOM - JUST EXIT                     
         SPACE 1                                                                
ADDNEG8  MVC   0(9,RE),SORTSSN                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PRINT LINEUP PATTERNS                                            
         SPACE 3                                                                
LINEUP   NTR1                                                                   
         L     R1,=F'111111111'                                                 
         LA    R0,2                PRINT 2 PATTERNS                             
         CLI   RECNUM,WS           IF GENERATING A SPOOL TAPE                   
         BNE   *+8                                                              
         LA    R0,4                PRINT 4 PATTERNS PER PAGE                    
         MVC   SORTSSN,=C'123456789'   DUMMY UP A SORT RECORD                   
         MVC   SORTSTAT,=C'NY '                                                 
         MVC   SORTLOCL,=C'NYC'                                                 
         MVC   EMPNAME,EXES                                                     
         MVC   EMPADD,EXES                                                      
         MVC   W4NAME,EXES                                                      
         MVC   W4ADD(1),EXES                                                    
         MVC   W4ADD+1(L'W4ADD-1),W4ADD                                         
         MVI   LUPSW,C'Y'                                                       
         SPACE 1                                                                
LINEUP2  ST    R1,SORTFERN                                                      
         ST    R1,SORTFTAX                                                      
         ST    R1,SORTSERN                                                      
         ST    R1,SORTSTAX                                                      
         ST    R1,SORTLERN                                                      
         ST    R1,SORTLTAX                                                      
         ST    R1,SORTFICA                                                      
         ST    R1,SORTSDI                                                       
         ST    R1,SORTSUI                                                       
         ST    R1,SORTSFLI                                                      
*        ST    R1,SORTNHA                                                       
*                                                                               
         CLI   RECNUM,WS           IF GENERATING A SPOOL TAPE                   
         BNE   LINEUP6                                                          
         BAS   RE,SPLPRINT         USE ITS FORM                                 
         BAS   RE,DOSPACE          TAKE CARE OF SPACING BETWEEN FORMS           
         B     LINEUP8                                                          
*                                                                               
LINEUP6  CLC   YEAR,=C'1992'       1992 AND PRIOR W2 RECORDS                    
         BH    *+12                                                             
         BAS   RE,PRINTW2          USE OLD STYLE FORMAT                         
         B     *+8                                                              
         BAS   RE,NEWPRINT         ELSE, USE NEW STYLE                          
*                                                                               
LINEUP8  A     R1,=F'11111111'                                                  
         BCT   R0,LINEUP2                                                       
         MVI   LUPSW,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              PRINT W2 FORMS TO SPOOL TAPE                                     
         SPACE 3                                                                
SPLPRINT NTR1                                                                   
         LA    R2,MYP                                                           
         USING PSPLD,R2                                                         
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   LINE,0                                                           
         SPACE 1                                                                
         BAS   RE,SPLLN1           LINE 1                                       
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLLN3N4         LINE 3 AND 4                                 
         BAS   RE,SPLLN5N6         LINE 5 AND 6                                 
         BAS   RE,SPLLN7           LINE 7                                       
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLLN9           LINE 9                                       
         BAS   RE,SPLLN10          LINES 10                                     
         BAS   RE,SPLLN11          LINE 11                                      
         BAS   RE,SPLLN123         LINE 12,13                                   
         BAS   RE,SPLLN14          LINE 14                                      
         BAS   RE,SPLLN15          LINE 15                                      
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLLN17          LINE 17                                      
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
SPLLN1   NTR1                                                                   
         OC    SORTTXRE,SORTTXRE                                                
         BNZ   SPLLN1A                                                          
         OC    SORTREXP,SORTREXP                                                
         BNZ   *+14                                                             
         OC    SORTFERN(8),SORTFERN                                             
         BZ    SPLLN1X                                                          
SPLLN1A  L     R1,SORTFERN                                                      
         A     R1,SORTREXP                                                      
         A     R1,SORTTXRE         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(14,PSFEARN),2          WAGES, TIPS, OTHER COMP.            
         EDIT  (4,SORTFTAX),(14,PSFTAX),2   FEDERAL INCOME TAX WITHHELD         
SPLLN1X  BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN3N4 NTR1                                                                   
*                                  LINE 3                                       
         EDIT  (4,FICAWAGE),(14,PSFWAGE),2  SOCIAL SECURITY WAGES               
         EDIT  (4,SORTFICA),(14,PSFICA),2   S.S TAX WITHHELD                    
         BAS   RE,SPLAT                                                         
*                                  LINE 4                                       
         MVC   PSEMPNM,EMPNAME              EMPLOYER'S NAME                     
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN5N6 NTR1                                                                   
*                                  LINE 5                                       
         MVC   PSEADD,EMPADD               EMPLOYER'S ADDRESS LINE 1            
*                                                                               
         EDIT  (4,MEDWAGE),(14,PSMWAGE),2  MEDICARE WAGES                       
         EDIT  (4,MEDTAX),(14,PSMTAX),2    MEDICARE TAX WITHHELD                
         BAS   RE,SPLAT                                                         
*                                  LINE 6                                       
         MVC   PSEADD2,EMPADD+30           EMPLOYER'S ADDRESS LINE 2            
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN7   NTR1                                                                   
         MVC   PSEADD3,EMPADD+60   EMPLOYER'S ADDRESS LINE 3                    
         CLC   YEAR,=C'2000'                                                    
         BH    SPLLN7A                                                          
         EDIT  (4,SORTREXP),(14,PSBENFIT),2  BENEFITS INCLUDED IN BOX 1         
SPLLN7A  BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
SPLLN9   NTR1                                                                   
         OC    SORTSDI,SORTSDI     SDI                                          
         BZ    SPLLN9X                                                          
         MVC   PSOTH(2),=C'DI'                                                  
         EDIT  (4,SORTSDI),(11,PSOTH+2),2                                       
SPLLN9X  BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN10  NTR1                                                                   
         MVC   PSW4NAME,W4NAME     EMPLOYEE'S NAME                              
         OC    SORTSUI,SORTSUI     SUI                                          
         BZ    SPLLN10X                                                         
         CLC   SORTSTAT,=C'MT '    IF MONTANA                                   
         BE    SPLLN10X            SHOW SUI IN LOCAL INCOME TAX                 
         MVC   PSOTH2(9),=C'UI/WF/SWF'                                          
         EDIT  (4,SORTSUI),(11,PSOTH2+9),2                                      
SPLLN10X BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN11  NTR1                                                                   
         MVC   PSW4ADD,W4ADD       EMPLOYEE'S ADDRESS - LINE 1                  
         MVC   PSEMPID,EXES                                                     
         CLI   LUPSW,C'Y'                                                       
         BE    *+10                                                             
         MVC   PSEMPID,EMPFDID     EMPLOYER'S FEDERAL I.D. NUMBER               
         SPACE 1                                                                
         OC    SORTREXP,SORTREXP                                                
         BNZ   SPLLN11X                                                         
         OC    SORTFERN(8),SORTFERN                                             
         BNZ   SPLLN11X                                                         
         MVC   PSTONLY,=CL12'(STATE ONLY)'                                      
*                                                                               
SPLLN11X BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
SPLLN123 NTR1                                                                   
*                                  LINE 12                                      
         MVC   PSW4ADD2,W4ADD+39   EMPLOYEE'S ADDRESS - LINE 2                  
         BAS   RE,SPLAT                                                         
*                                  LINE 13                                      
         MVC   PSW4ADD3,W4ADD+78   EMPLOYEE'S ADDRESS - LINE 3                  
         LA    R1,PSW4SSN          EMPLOYEE'S SSN                               
         BAS   RE,GETSSN                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN14  NTR1                                                                   
         MVC   PSW4ADD4,W4ADD+117  EMPLOYEE'S ADDRESS - LINE 4                  
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN15  NTR1                                                                   
         CLC   =C'PP ',SORTKEMP    EMPLOYER PP NEVER HAS PENSION                
         BE    SPLLN15X                                                         
         CLC   =C'P+ ',SORTKEMP    EMPLOYER P+ NEVER HAS PENSION                
         BE    SPLLN15X                                                         
         MVI   PSPP,C'X'           PENSION INDCATOR                             
         TM    NEEDSTA2,TAW4SNPE                                                
         BNO   *+8                                                              
         MVI   PSPP,C' '           OR NOT                                       
SPLLN15X BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLLN17  NTR1                                                                   
         CLC   LASTSTAT,SORTSTAT   IF STATE WAS PRINTED BEFORE                  
         BE    SPLLN17M            SKIP TO LOCAL                                
         MVC   LASTSTAT,SORTSTAT                                                
*                                                                               
         MVC   TIDUNIT,SORTSTAT                                                 
         BAS   RE,GETTID                                                        
         MVC   PSSID,TIDID         EMPLOYER'S STATE I.D. NUMBER                 
         CLI   SORTSTAT,0               IF STATE DETAILS                        
         BE    SPLLN17L                                                         
         MVC   PSSTATE,SORTSTAT    STATE                                        
         L     R1,SORTSERN                                                      
         A     R1,SORTSTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(12,PSSEARN),2          STATE WAGES, TIPS, ETC              
**NO-OP  EDIT  (4,SORTSERN),(12,PSSEARN),2  STATE WAGES, TIPS, ETC              
         EDIT  (4,SORTSTAX),(11,PSSTAX),2   STATE INCOME TAXES                  
         SPACE 1                                                                
SPLLN17L CLC   SORTSTAT,=C'MT '    IF MONTANA                                   
         BNE   SPLLN17M                                                         
         MVC   PSLOCLNM,=CL8'OFLT'         HARD CARD LOCALITY NAME              
         L     R1,SORTSERN                                                      
         A     R1,SORTSTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(10,PSLEARN),2         SHOW ST WAGES IN LOC WAGES           
**NO-OP  EDIT  (4,SORTSERN),(10,PSLEARN),2 SHOW ST WAGES IN LOC WAGES           
         EDIT  (4,SORTSUI),(9,PSLTAX),2    SHOW SUI IN LOCAL INCOME TX          
         B     SPLLN17X                                                         
*                                                                               
SPLLN17M CLI   SORTLOCL,0               IF LOCAL DETAILS                        
         BE    SPLLN17X                                                         
         MVC   TGTANAME,MYSPACES                                                
         GOTO1 TAXVAL,DMCB,(3,SORTLOCL)                                         
         MVC   PSLOCLNM,TGTANAME        LOCALITY NAME                           
         L     R1,SORTLERN                                                      
         A     R1,SORTLTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(10,PSLEARN),2          LOCAL WAGES,TIPS, ETC               
**NO-OP  EDIT  (4,SORTLERN),(10,PSLEARN),2  LOCAL WAGES,TIPS, ETC               
         EDIT  (4,SORTLTAX),(9,PSLTAX),2    LOCAL INCOME TAX                    
         SPACE 1                                                                
SPLLN17X BAS   RE,SPLAT                                                         
         B     XIT                      PRINT AND EXIT                          
         DROP  R2                                                               
         EJECT                                                                  
*              PRINT W2 FORMS                                                   
PRINTW2  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'N'       ENSURE WE DON'T SKIP                         
         MVI   LINE,0                                                           
         DROP  R5                                                               
         SPACE 1                                                                
         BAS   RE,SPLAT            LINE 1                                       
         BAS   RE,SPLAT            LINE 2                                       
*                                                                               
         CLI   SORTPRNT,C'D'       FOR REPRINTS                                 
         BNE   *+10                                                             
         MVC   MYP+7(19),=C'DUPLICATE STATEMENT'                                
         BAS   RE,SPLAT            LINE 3                                       
*                                                                               
         BAS   RE,SPLAT            LINE 4                                       
         BAS   RE,SPLAT            LINE 5                                       
         BAS   RE,SPLAT            LINE 6                                       
*                                                                               
         MVC   MYP+7(30),EMPNAME                                                
         CLI   W4TYPE,C'E'         ESTATE = DECEASED                            
         BNE   *+8                                                              
         MVI   MYP+44,C'X'                                                      
         CLC   =C'PP ',SORTKEMP    UNLESS EMPLOYER PP                           
         BE    PW07                                                             
         CLC   =C'P+ ',SORTKEMP    OR UNLESS EMPLOYER P+                        
         BE    PW07                                                             
         TM    NEEDSTA2,TAW4SNPE   OR UNLESS NO PENSION PLAN                    
         BO    PW07                                                             
         MVI   MYP+48,C'X'         INDICATE PENSION PLAN                        
PW07     BAS   RE,SPLAT            LINE 7                                       
*                                                                               
         MVC   MYP+7(30),EMPADD                                                 
         BAS   RE,SPLAT            LINE 8                                       
*                                                                               
         MVC   MYP+7(30),EMPADD+30                                              
         BAS   RE,SPLAT            LINE 9                                       
*                                                                               
         MVC   MYP+7(30),EMPADD+60                                              
         BAS   RE,SPLAT            LINE 10                                      
*                                                                               
         MVC   MYP+7(30),EMPADD+90                                              
         MVC   MYP+40(12),=C'(STATE ONLY)'                                      
         OC    SORTREXP,SORTREXP                                                
         BNZ   *+14                                                             
         OC    SORTFERN(8),SORTFERN                                             
         BZ    PWFED2                                                           
         EDIT  (4,SORTFTAX),(14,MYP+40),2                                       
         L     R1,SORTFERN                                                      
         A     R1,SORTREXP                                                      
         EDIT  (R1),(14,MYP+56),2                                               
PWFED2   BAS   RE,SPLAT            LINE 11                                      
         BAS   RE,SPLAT            LINE 12                                      
*                                                                               
         MVC   MYP+7(14),EXES                                                   
         CLI   LUPSW,C'Y'                                                       
         BE    *+10                                                             
         MVC   MYP+7(14),EMPFDID   FEDERAL TAX ID                               
         MVC   TIDUNIT,SORTSTAT                                                 
         BAS   RE,GETTID                                                        
         MVC   MYP+23(14),TIDID    STATE TAX ID                                 
**NO-OP**BAL   RE,GETFICA          GET FICA INFO                                
         EDIT  (4,SORTFICA),(14,MYP+40),2                                       
         EDIT  (4,FICAWAGE),(14,MYP+56),2                                       
         BAS   RE,SPLAT            LINE 13                                      
         BAS   RE,SPLAT            LINE 14                                      
*                                                                               
         LA    R1,MYP+7                                                         
         BAS   RE,GETSSN                                                        
         EDIT  (4,MEDWAGE),(14,MYP+56),2                                        
         BAS   RE,SPLAT            LINE 15                                      
         BAS   RE,SPLAT            LINE 16                                      
*                                                                               
         MVC   MYP+7(30),W4NAME                                                 
         EDIT  (4,MEDTAX),(14,MYP+40),2                                         
         BAS   RE,SPLAT            LINE 17                                      
*                                                                               
         MVC   MYP+7(39),W4ADD                                                  
         BAS   RE,SPLAT            LINE 18                                      
*                                                                               
         MVC   MYP+7(39),W4ADD+39                                               
         OC    SORTSDI,SORTSDI     SHOW SDI IF AROUND                           
         BZ    PW19                                                             
         MVC   MYP+55(3),=C'SDI'                                                
         EDIT  (4,SORTSDI),(12,MYP+58),2                                        
PW19     BAS   RE,SPLAT            LINE 19                                      
*                                                                               
         MVC   MYP+7(39),W4ADD+78                                               
         OC    SORTSUI,SORTSUI     SHOW SUI IF AROUND                           
         BZ    PW20                                                             
         MVC   MYP+55(3),=C'SUI'                                                
         EDIT  (4,SORTSUI),(12,MYP+58),2                                        
PW20     BAS   RE,SPLAT            LINE 20                                      
*                                                                               
         MVC   MYP+7(39),W4ADD+117                                              
         BAS   RE,SPLAT            LINE 21                                      
         BAS   RE,SPLAT            LINE 22                                      
         BAS   RE,SPLAT            LINE 23                                      
*                                                                               
         EDIT  (4,SORTREXP),(14,MYP+56),2                                       
         BAS   RE,SPLAT            LINE 24                                      
         BAS   RE,SPLAT            LINE 25                                      
*                                                                               
         CLI   SORTSTAT,0          STATE DETAILS                                
         BE    PW22                                                             
         CLC   LASTSTAT,SORTSTAT   IF STATE WAS PRINTED BEFORE                  
         BE    PW22                SKIP TO LOCAL                                
         MVC   LASTSTAT,SORTSTAT                                                
                                                                                
         EDIT  (4,SORTSTAX),(10,MYP+6),2                                        
         EDIT  (4,SORTSERN),(10,MYP+17),2                                       
         MVC   TGTANAME,MYSPACES                                                
         GOTO1 TAXVAL,DMCB,(3,SORTSTAT)                                         
         MVC   MYP+29(8),TGTANAME                                               
PW22     CLI   SORTLOCL,0          LOCAL DETAILS                                
         BE    PW24                                                             
         EDIT  (4,SORTLTAX),(10,MYP+39),2                                       
         EDIT  (4,SORTLERN),(10,MYP+50),2                                       
         MVC   TGTANAME,MYSPACES                                                
         GOTO1 TAXVAL,DMCB,(3,SORTLOCL)                                         
         MVC   MYP+62(8),TGTANAME                                               
PW24     BAS   RE,SPLAT            LINE 26                                      
         CLI   SKIPOPT,C'Y'        PRINT 1 W2 PER PAGE                          
         BNE   PW25                                                             
         BAS   RE,NEWPAGE                                                       
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
PW25     CLI   SKIPSW,0            EVERY ODD FORM, SPACE DOWN                   
         BNE   PW26                                                             
         MVI   SKIPSW,1                                                         
         BAS   RE,SPLAT            LINE 27                                      
         BAS   RE,SPLAT            LINE 28                                      
         BAS   RE,SPLAT            LINE 29                                      
         BAS   RE,SPLAT            LINE 30                                      
         BAS   RE,SPLAT            LINE 31                                      
         BAS   RE,SPLAT            LINE 32                                      
         BAS   RE,SPLAT            LINE 33                                      
         B     XIT                                                              
*                                                                               
PW26     MVI   SKIPSW,0            EVERY EVEN FORM, SKIP TO CHANNEL 1           
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINT NEW STYLE W2 FORMS                                         
         SPACE 3                                                                
NEWPRINT NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'N'       ENSURE WE DON'T SKIP                         
         MVI   LINE,0                                                           
         DROP  R5                                                               
         SPACE 1                                                                
         BAS   RE,SPLAT            LINE 1                                       
         BAS   RE,SPLAT            LINE 2                                       
*                                                                               
         CLI   SORTPRNT,C'D'       FOR REPRINTS                                 
         BNE   *+10                                                             
         MVC   MYP+7(19),=C'DUPLICATE STATEMENT'                                
         BAS   RE,SPLAT            LINE 3                                       
*                                                                               
         BAS   RE,SPLAT            LINE 4                                       
         BAS   RE,SPLAT            LINE 5                                       
         BAS   RE,SPLAT            LINE 6                                       
*                                                                               
         BAS   RE,PRTLN7           PRINT LINE 7                                 
         BAS   RE,SPLAT            LINE 8                                       
         BAS   RE,PRTLN9           PRINT LINE 9                                 
*                                                                               
         MVC   MYP+7(30),EMPADD    EMPLOYER ADDRESS                             
         BAS   RE,SPLAT            LINE 10                                      
*                                                                               
         BAS   RE,PRTLN11          PRINT LINE 11                                
*                                                                               
         MVC   MYP+7(30),EMPADD+60 EMPLOYER'S ADDRESS (CONTINUED)               
         BAS   RE,SPLAT            LINE 12                                      
*                                                                               
         MVC   MYP+7(30),EMPADD+90 EMPLOYER'S ADDRESS (CONTINUED)               
         BAS   RE,SPLAT            LINE 13                                      
         BAS   RE,SPLAT            LINE 14                                      
*                                                                               
         LA    R1,MYP+7            EMPLOYEE'S SOCIAL SECURITY NUMBER            
         BAS   RE,GETSSN                                                        
         BAS   RE,SPLAT            LINE 15                                      
         BAS   RE,SPLAT            LINE 16                                      
*                                                                               
         BAS   RE,PRTLN17          PRINT LINE 17                                
*                                                                               
         MVC   MYP+7(39),W4ADD     EMPLOYEE'S ADDRESS                           
         BAS   RE,SPLAT            LINE 18                                      
*                                                                               
         BAS   RE,PRTLN19          PRINT LINE 19                                
         BAS   RE,PRTLN20          PRINT LINE 20                                
         BAS   RE,PRTLN21          PRINT LINE 21                                
*                                                                               
         BAS   RE,SPLAT            LINE 22                                      
         BAS   RE,SPLAT            LINE 23                                      
*                                                                               
         CLI   W4TYPE,C'E'         ESTATE = DECEASED                            
         BNE   NEWPW20                                                          
         CLC   YEAR,=C'1998'                                                    
         BL    *+12                                                             
         MVI   MYP+47,C'X'                                                      
         B     *+8                                                              
         MVI   MYP+45,C'X'                                                      
*                                                                               
NEWPW20  CLC   =C'PP ',SORTKEMP    UNLESS EMPLOYER PP                           
         BE    NEWPW24                                                          
         CLC   =C'P+ ',SORTKEMP    OR UNLESS EMPLOYER P+                        
         BE    NEWPW24                                                          
         TM    NEEDSTA2,TAW4SNPE   OR UNLESS NO PENSION PLAN                    
         BO    NEWPW24                                                          
         CLC   YEAR,=C'1998'                                                    
         BL    *+12                                                             
         MVI   MYP+53,C'X'         INDICATE PENSION                             
         B     *+8                                                              
         MVI   MYP+49,C'X'         INDICATE PENSION PLAN                        
NEWPW24  BAS   RE,SPLAT            LINE 24                                      
         BAS   RE,SPLAT            LINE 25                                      
*                                                                               
         BAS   RE,PRTLN26          PRINT LINE 26                                
*                                                                               
         CLI   SKIPOPT,C'Y'        PRINT 1 W2 PER PAGE                          
         BNE   NEWPW25                                                          
         BAS   RE,NEWPAGE                                                       
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
NEWPW25  CLI   SKIPSW,0            EVERY ODD FORM, SPACE DOWN                   
         BNE   NEWPW26                                                          
         MVI   SKIPSW,1                                                         
         BAS   RE,SPLAT            LINE 27                                      
         BAS   RE,SPLAT            LINE 28                                      
         BAS   RE,SPLAT            LINE 29                                      
         BAS   RE,SPLAT            LINE 30                                      
         BAS   RE,SPLAT            LINE 31                                      
         BAS   RE,SPLAT            LINE 32                                      
         BAS   RE,SPLAT            LINE 33                                      
         B     XIT                                                              
*                                                                               
NEWPW26  MVI   SKIPSW,0            EVERY EVEN FORM, SKIP TO CHANNEL 1           
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT LINE 7 ON NEW STYLE FORM                        
         SPACE                                                                  
PRTLN7   NTR1                                                                   
*                                  EMPLOYER'S IDENTIFICATION NUMBER             
         MVC   MYP+7(14),EXES                                                   
         CLI   LUPSW,C'Y'                                                       
         BE    *+10                                                             
         MVC   MYP+7(14),EMPFDID                                                
*                                                                               
         MVC   MYP+56(12),=C'(STATE ONLY)'                                      
         OC    SORTTXRE,SORTTXRE                                                
         BNZ   PRTLN7A                                                          
         OC    SORTREXP,SORTREXP                                                
         BNZ   *+14                                                             
         OC    SORTFERN(8),SORTFERN                                             
         BZ    PRTLN7X                                                          
*                                  WAGES,TIPS, OTHER COMPENSATION               
PRTLN7A  L     R1,SORTFERN                                                      
         A     R1,SORTREXP                                                      
         A     R1,SORTTXRE         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(14,MYP+40),2                                               
*                                  FEDERAL INCOME TAX WITHHELD                  
         EDIT  (4,SORTFTAX),(14,MYP+56),2                                       
*                                                                               
PRTLN7X  BAS   RE,SPLAT            PRINT LINE 7                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE 9 ON NEW STYLE FORM                        
         SPACE 1                                                                
PRTLN9   NTR1                                                                   
*                                  EMPLOYER'S NAME                              
         MVC   MYP+7(30),EMPNAME                                                
*                                                                               
**NO-OP**BAL   RE,GETFICA          GET FICA INFO                                
*                                  SOCIAL SECURITY WAGES                        
         EDIT  (4,FICAWAGE),(14,MYP+40),2                                       
*                                  SOCIAL SECURITY TAX WITHHELD                 
         EDIT  (4,SORTFICA),(14,MYP+56),2                                       
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT LINE 11 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN11  NTR1                                                                   
*                                  EMPLOYER'S ADDRESS (CONTINUED)               
         MVC   MYP+7(30),EMPADD+30                                              
*                                  MEDICARE WAGES AND TIPS                      
         EDIT  (4,MEDWAGE),(14,MYP+40),2                                        
*                                  MEDICARE TAX WITHHELD                        
         EDIT  (4,MEDTAX),(14,MYP+56),2                                         
         BAS   RE,SPLAT            LINE 11                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE 17 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN17  NTR1                                                                   
*                                  EMPLOYEE'S NAME                              
         MVC   MYP+7(30),W4NAME                                                 
*                                  BENEFITS INCLUDED IN BOX 1                   
*        CLC   YEAR,=C'2000'       2001 W2 DOES NOT HAVE THIS                   
*        BH    PRTLN17A                                                         
*        EDIT  (4,SORTREXP),(14,MYP+56),2                                       
                                                                                
*        OC    SORTNHA,SORTNHA     SHOW NHA IF AROUND                           
*        BZ    PRTLN17A                                                         
*        MVC   MYP+56(2),=C'CC'    NHA                                          
*        EDIT  (4,SORTNHA),(11,MYP+66),2                                        
*                                                                               
PRTLN17A BAS   RE,SPLAT            LINE 17                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT LINE 19 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN19  NTR1                                                                   
*                                  EMPLOYEE'S ADDRESS (CONTINUED)               
         MVC   MYP+7(39),W4ADD+39                                               
*                                  'OTHERS' BOX                                 
         OC    SORTSDI,SORTSDI     SHOW SDI IF AROUND                           
         BZ    PRTLN19X                                                         
         MVC   MYP+56(2),=C'DI'                                                 
         EDIT  (4,SORTSDI),(11,MYP+66),2                                        
*                                                                               
PRTLN19X BAS   RE,SPLAT            LINE 19                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE 20 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN20  NTR1                                                                   
*                                  EMPLOYEE'S ADDRESS (CONTINUED)               
         MVC   MYP+7(39),W4ADD+78                                               
*                                  'OTHERS' BOX                                 
         OC    SORTSUI,SORTSUI     SHOW SUI IF AROUND                           
         BZ    PRTLN20X                                                         
         CLC   SORTSTAT,=C'MT '    IF MONTANA                                   
         BE    PRTLN20X            SHOW SUI IN LOCAL INCOME TAX                 
         MVC   MYP+56(9),=C'UI/WF/SWF'                                          
****     MVC   MYP+56(2),=C'UI'                                                 
         EDIT  (4,SORTSUI),(11,MYP+66),2                                        
*&&DO                                                                           
         CLC   YEAR,=C'1994'       ONLY FOR 1994 AND LATER                      
         BL    PRTLN20X                                                         
         CLC   SORTSTAT,=C'NJ '    IF NJ, USE NEW LABEL                         
         BNE   *+10                                                             
         MVC   MYP+56(5),=C'WD HC'                                              
*&&                                                                             
*                                                                               
PRTLN20X BAS   RE,SPLAT            LINE 20                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT LINE 21 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN21  NTR1                                                                   
*                                  EMPLOYEE'S ADDRESS (CONTINUED)               
         MVC   MYP+7(39),W4ADD+117                                              
*                                  'OTHERS' BOX                                 
         OC    SORTSFLI,SORTSFLI    SHOW FLI IF AROUND                          
         BZ    PRTLN21X                                                         
         MVC   MYP+56(3),=C'FLI'                                                
         EDIT  (4,SORTSFLI),(11,MYP+66),2                                       
*                                                                               
PRTLN21X BAS   RE,SPLAT            LINE 21                                      
         B     XIT                                                              
*              ROUTINE TO PRINT LINE 26 ON NEW STYLE FORM                       
         SPACE 1                                                                
PRTLN26  NTR1                                                                   
*                                                                               
         CLC   LASTSTAT,SORTSTAT   IF STATE WAS PRINTED BEFORE                  
         BE    PRTLN266            SKIP TO LOCAL                                
         MVC   LASTSTAT,SORTSTAT                                                
*                                  EMPLOYER'S STATE ID NO                       
         MVC   TIDUNIT,SORTSTAT                                                 
         BAS   RE,GETTID                                                        
         MVC   MYP+10(14),TIDID    STATE TAX ID                                 
         CLC   SORTSTAT,=C'WI '    IF WISCONSIN                                 
         BNE   PRTLN263                                                         
         MVI   MYP+10,C'0'                                                      
         MVC   MYP+11(14),TIDID    STATE TAX ID                                 
*                                                                               
PRTLN263 CLI   SORTSTAT,0          STATE DETAILS                                
         BE    PRTLN265                                                         
*                                  STATE CODE                                   
         MVC   MYP+7(L'SORTSTAT),SORTSTAT                                       
*                                  STATE WAGES,TIPS,ETC.                        
         L     R1,SORTSERN                                                      
         A     R1,SORTSTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(10,MYP+25),2                                               
**NO-OP  EDIT  (4,SORTSERN),(10,MYP+25),2                                       
*                                  STATE INCOME TAXES                           
         EDIT  (4,SORTSTAX),(9,MYP+35),2                                        
*                                                                               
PRTLN265 CLC   SORTSTAT,=C'MT '    IF MONTANA                                   
         BNE   PRTLN266                                                         
         MVC   MYP+45(8),=CL8'OFLT'        HARD CARD LOCALITY NAME              
         L     R1,SORTSERN                                                      
         A     R1,SORTSTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(10,MYP+52),2          SHOW ST WAGES IN LOC WAGES           
**NO-OP  EDIT  (4,SORTSERN),(10,MYP+52),2  SHOW ST WAGES IN LOC WAGES           
         EDIT  (4,SORTSUI),(9,MYP+62),2    SHOW SUI IN LOCAL INCOME TX          
         B     PRTLN26X                                                         
*                                                                               
PRTLN266 CLI   SORTLOCL,0          LOCAL DETAILS                                
         BE    PRTLN26X                                                         
         MVC   TGTANAME,MYSPACES                                                
         GOTO1 TAXVAL,DMCB,(3,SORTLOCL)                                         
         MVC   MYP+45(8),TGTANAME                                               
*                                  LOCAL WAGES,TIPS,ETC.                        
         L     R1,SORTLERN                                                      
         A     R1,SORTLTXR         TAXABLE REIMBURSEMENTS                       
         EDIT  (R1),(10,MYP+52),2                                               
**NO-OP  EDIT  (4,SORTLERN),(10,MYP+52),2                                       
*                                  LOCAL INCOME TAXES                           
         EDIT  (4,SORTLTAX),(9,MYP+62),2                                        
*                                                                               
PRTLN26X BAS   RE,SPLAT            LINE 26                                      
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR W2 PRINTING                              
         SPACE 3                                                                
GETTID   NTR1                                                                   
         CLI   LUPSW,C'Y'                                                       
         BNE   GETTID1                                                          
         MVC   TIDID,EXES                                                       
         B     XIT                                                              
         SPACE 1                                                                
GETTID1  MVC   TIDID,MYSPACES      PASS TIDUNIT - RETURN TIDID                  
         L     R6,ATHISEX          PT TO EMTAX RECORD                           
*                                                                               
         CLI   0(R6),TLEXCDQ       IS RECORD STILL THERE?                       
         BE    *+8                                                              
         L     R6,AIO2             NO, USE THE BACKUP IN AIO2                   
*                                                                               
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GETTID2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TATID,R6                                                         
         CLC   TATIUNIT,TIDUNIT                                                 
         BNE   GETTID2                                                          
         MVC   TIDID,TATIID                                                     
         OC    TIDID,MYSPACES                                                   
         B     XIT                                                              
         SPACE 1                                                                
TIDUNIT  DS    CL3                                                              
TIDID    DS    CL14                                                             
         SPACE 1                                                                
GETSSN   MVC   0(3,R1),SORTSSN                                                  
         MVI   3(R1),C'-'                                                       
         MVC   4(2,R1),SORTSSN+3                                                
         MVI   6(R1),C'-'                                                       
         MVC   7(4,R1),SORTSSN+5                                                
*                                                                               
         CLI   SORTSSN,C'9'        SSN STARTING 9 IS INVALID                    
         BLR   RE                                                               
         MVC   0(11,R1),=C'000-00-0000'                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO GET FICA INFO                                         
         SPACE 3                                                                
GETFICA  NTR1                                                                   
         LA    R2,LIMBLOCK                                                      
         XCEF  (R2),'TMLNQ'        CLEAR INTERFACE TO TALIM                     
         USING TMD,R2                                                           
         ST    RC,TMRC                                                          
         MVC   WORK(2),TIFYEAR                                                  
         MVC   WORK+2(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(0,TMEFDTE0)  SET INTERNAL FORMAT           
         MVC   TMEMP,TIEMP                                                      
         MVC   TMUNIT,=C'FD'                                                    
         OI    TMSTAT,TMSBSRT                                                   
         GOTO1 =V(TALIM),DMCB,(R2)                                              
         SPACE 1                                                                
         L     R1,SORTFERN                                                      
         A     R1,SORTTXRE         TAXABLE REIMBURSEMENTS                       
         ST    R1,FICAWAGE                                                      
**NO-OP  MVC   FICAWAGE,SORTFERN                                                
         OC    TMBFICA,TMBFICA     IF NO FICA WAGE MAX RETURNED                 
         BZ    GETFICA5            USE EARNINGS (RE-PRINTING OLD W2)            
**NO-OP  CLC   SORTFERN,TMBFICA                                                 
         CLC   FICAWAGE,TMBFICA                                                 
         BL    *+10                                                             
         MVC   FICAWAGE,TMBFICA                                                 
         SPACE 1                                                                
GETFICA5 L     R1,SORTFERN                                                      
         A     R1,SORTTXRE         TAXABLE REIMBURSEMENTS                       
         ST    R1,MEDWAGE                                                       
**NO-OP5 MVC   MEDWAGE,SORTFERN                                                 
**NO-OP  CLC   SORTFERN,TMBMED                                                  
         CLC   MEDWAGE,TMBMED                                                   
         BL    *+10                                                             
         MVC   MEDWAGE,TMBMED                                                   
                                                                                
         L     R1,MEDWAGE          COMPUTE MEDICARE TAX                         
         M     R0,TMRMED                                                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         C     R1,SORTFICA         INSURE NOT > TOTAL                           
         BNH   *+8                                                              
         L     R1,SORTFICA                                                      
         ST    R1,MEDTAX                                                        
                                                                                
         CLC   MEDWAGE,=F'20000000'      IF MEDICARE WAGES > 200,000            
         BNH   GETFICA7                                                         
         XR    R0,R0                                                            
         L     R1,MEDWAGE                                                       
         S     R1,=F'20000000'                                                  
         M     R0,=F'900'          2.35% - 1.45% = 0.90%                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         L     R0,MEDTAX                                                        
         AR    R1,R0                                                            
         ST    R1,MEDTAX                                                        
                                                                                
GETFICA7 L     R0,SORTFICA         FICA = TOTAL - MEDICARE                      
         SR    R0,R1                                                            
         ST    R0,SORTFICA                                                      
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 1                                                                
         DS    0F                                                               
LIMBLOCK DS    0C                                                               
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
NEEDEM   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     FIRST READ EMTAX RECORD                      
         LA    R4,NEEDKEY                                                       
         USING TLEXD,R4                                                         
         MVI   TLEXCD,TLEXCDQ                                                   
         MVC   TLEXEMP,SORTKEMP                                                 
         CLC   YEAR,=C'2004'                                                    
         BNL   NEEDEM3                                                          
         CLC   TLEXEMP,=C'TP '                                                  
         BNE   NEEDEM3                                                          
         MVC   TLEXEMP,=C'TP1'     USE TP IDS FROM 2003                         
*                                                                               
NEEDEM3  BAS   RE,NEEDREC                                                       
         MVC   ATHISEX,NEEDAREC    SAVE ADDRESS OF RECORD                       
         L     RE,ATHISEX                                                       
         LA    R1,1000                                                          
         L     RF,AIO2                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
*                                  THEN READ EMP REC FOR NAME & ADDR            
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,SORTKEMP                                                 
         CLC   YEAR,=C'2004'                                                    
         BNL   NEEDEM5                                                          
         CLC   TLEMEMP,=C'TP '                                                  
         BNE   NEEDEM5                                                          
         MVC   TLEMEMP,=C'TP1'     USE TP IDS FROM 2003                         
*                                                                               
NEEDEM5  BAS   RE,NEEDREC                                                       
*                                  USE EMP INFO AS PER IRV 4/8/93               
         CLC   TLEMEMP(2),=C'TP'                                                
         BNE   NEEDEMX                                                          
         CLC   YEAR,=C'2004'                                                    
         BNL   NEEDTP                                                           
**NO-OP* CLC   TLEMEMP(2),=C'DM'                                                
**NO-OP* BE    NEEDDM                                                           
**NO-OP* CLC   TLEMEMP(2),=C'PG'                                                
**NO-OP* BE    NEEDPG                                                           
         B     NEEDEMX                                                          
         SPACE 1                                                                
NEEDDM   MVC   NEEDNAME,=CL36'ADVERTISING TALENT SERVICES'                      
         B     NEEDEMX                                                          
NEEDTP   MVC   NEEDNAME,=CL36'TALENT PARTNERS COM SERV, LLC'                    
         B     NEEDEMX                                                          
         SPACE 1                                                                
NEEDPG   MVC   NEEDNAME,=CL36'PROCTOR && GAMBLE PRODUCTIONS INC.'               
         MVC   NEEDADD(1),MYSPACES                                              
         MVC   NEEDADD+1(L'NEEDADD-1),NEEDADD                                   
         MVC   NEEDADD(30),=CL30'1 PROCTOR && GAMBLE PLAZA'                     
         MVC   NEEDADD+30(30),=CL30'CINCINNATI  OHIO 45202'                     
*                                                                               
NEEDEMX  MVC   EMPNAME,NEEDNAME                                                 
         MVC   EMPADD,NEEDADD                                                   
*                                                                               
         MVC   EMPFDID,MYSPACES    SAVE FEDERAL UNEMPLOYMENT ID                 
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         MVC   AIO,NEEDAREC                                                     
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BNE   XIT                                                              
         L     R6,TGELEM                                                        
         USING TATID,R6                                                         
         MVC   EMPFDID,TATIID                                                   
         B     XIT                                                              
         SPACE 1                                                                
NEEDW4   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,SORTSSN                                                  
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         CLI   LUPSW,C'Y'                                                       
         BNE   NREC1                                                            
         MVC   NEEDNAME,EXES                                                    
         MVC   NEEDADD(1),EXES                                                  
         MVC   NEEDADD+1(L'NEEDADD-1),NEEDADD                                   
         B     XIT                                                              
         SPACE 1                                                                
NREC1    L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'150'     SET UP FOR 150 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,150                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEF                                                                   
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'600016'           (150*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         BAS   RE,GETADD                                                        
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL50                                                             
NEEDADD  DS    CL(4*39)                                                         
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
NEEDSTA2 DS    CL1                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(16),TAW4NAM1                                            
         CLI   TAW4LEN,TAW4LN2Q   HAVE MIDDLE NAME?                             
         BE    GW4NM10                                                          
         MVC   NEEDNAME+17(16),TAW4NAM2                                         
         GOTO1 SQUASHER,DMCB,NEEDNAME,33                                        
         B     GW4NM20                                                          
GW4NM10  MVC   NEEDNAME+17(16),TAW4MIDN                                         
         MVC   NEEDNAME+34(16),TAW4NAM2                                         
         GOTO1 SQUASHER,DMCB,NEEDNAME,50                                        
GW4NM20  MVC   NEEDTYPE,TAW4TYPE                                                
         MVC   NEEDSTA2,TAW4STA2                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETADD   NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDADD(1),MYSPACES                                              
         MVC   NEEDADD+1(L'NEEDADD-1),NEEDADD                                   
         SPACE                                                                  
         CLI   0(R4),TLW4CDQ       TEST HAVE W4 RECORD                          
         BNE   GETADD10                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TAA2ELQ      TEST FOR NEW STYLE ADDRESS                   
         BAS   RE,GETEL                                                         
         BNE   GETADD7                                                          
         USING TAA2D,R6                                                         
         LA    R2,NEEDADD          R2=WHERE TO SAVE IT IN NEEDADD               
         LA    R4,TAA2ADD1                                                      
         LHI   R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    GETADD2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    GETADD2                                                          
         LHI   R0,2                ELSE USE 2                                   
GETADD2  CLC   0(30,R4),MYSPACES   TEST FOR ANY DATA LEFT                       
         BNH   GETADD5                                                          
         MVC   0(30,R2),0(R4)                                                   
         LA    R2,39(R2)           BUMP IN NEEDADD (LEAVE ROOM FOR 39)          
         LA    R4,30(R4)           BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETADD2                                                       
         SPACE                                                                  
GETADD5  XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY      CITY                                      
         MVC   WORK+26(2),TAA2ST      STATE                                     
         MVC   WORK+29(10),TAA2ZIP    ZIP                                       
         GOTO1 SQUASHER,DMCB,WORK,39  SQUASH IT                                 
         MVC   0(39,R2),WORK                                                    
         SPACE                                                                  
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    GETADD6                                                          
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BE    GETADD6                                                          
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   GETADD6                                                          
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   39(0,R2),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    39(0,R2),MYSPACES                                                
         DROP  R1                                                               
         SPACE                                                                  
GETADD6  MVC   ELCODE,SAVEEL                                                    
         B     XIT                                                              
         SPACE                                                                  
         USING TAADD,R6                                                         
GETADD7  MVI   ELCODE,TAADELQ      OLD STYLE ADDRESS                            
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         ZIC   R0,TAADLNES         NUMBER OF ADDRESS LINES                      
         LA    R2,NEEDADD          R2=WHERE TO SAVE IT IN NEEDADD               
         LA    R4,TAADADD                                                       
GETADD8  MVC   0(30,R2),0(R4)                                                   
         LA    R2,39(R2)           BUMP IN NEEDADD (LEAVE ROOM FOR 39)          
         LA    R4,30(R4)           BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETADD8                                                       
         B     XIT                                                              
         SPACE                                                                  
GETADD10 MVI   ELCODE,TAADELQ      ADDRESS                                      
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDADD(0),TAADADD                                               
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R1,DRONE            USING UNUSED CORERES AREA                    
         CLI   0(R1),X'90'         TO SAVE RESULTS FILE NUMBER                  
         BNE   *+8                 BETWEEN REQUESTS                             
         MVI   0(R1),0                                                          
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         MVC   WORK(20),=CL20'TALTAPE.TA0W2DS1'                                 
         GOTO1 DYNALLOC,DMCB,(0,=CL8'W2TAPE'),((RF),WORK)                       
         L     R2,=A(W2TAPE)                                                    
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         BAS   RE,SPLAT                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         EDIT  (P6,TAPCOUNT),(7,MYP)                                            
         MVC   MYP+8(12),=C'TAPE RECORDS'                                       
         DROP  R2                                                               
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         L     R2,=A(W2TAPE)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         AP    TAPCOUNT,=P'1'                                                   
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         CLI   TRACOPT,C'Y'                                                     
         BNE   PUTTAPE2                                                         
         CP    TAPCOUNT,TRALIMIT                                                
         BH    PUTTAPE2                                                         
         BAS   RE,TRACEM                                                        
         SPACE 1                                                                
PUTTAPE2 LA    R5,TAPEIO                                                        
         L     R1,=A(W2TAPE)                                                    
         PUT   (1),(5)                                                          
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEM   NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,TAPEIO                                                        
         LA    R0,7                                                             
         SPACE 1                                                                
TRACE2   MVC   MYP(100),0(R2)                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,100(R2)                                                       
         BCT   R0,TRACE2                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         CP    W2COUNT,TRALIMIT                                                 
         BH    *+8                                                              
         BAS   RE,TRACEM                                                        
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         SPACE 1                                                                
         DS    0F                                                               
LASTSKEY DC    XL24'00'                                                         
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(96)'                                  
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO TAKE CARE OF SPACING FOR WS REPORT                    
         SPACE 1                                                                
DOSPACE  NTR1                                                                   
         CH    R0,=H'4'            IF JUST FINISHED FIRST FORM                  
         BNE   *+8                                                              
         BAS   RE,SPLAT5           SKIP FIVE LINES                              
         CH    R0,=H'3'            IF JUST FINISHED SECOND FORM                 
         BNE   *+8                                                              
         BAS   RE,SPLAT5           SKIP FIVE LINES                              
         CH    R0,=H'2'            IF JUST FINISHED THIRD FORM                  
         BNE   *+8                                                              
         BAS   RE,SPLAT2           SKIP TWO LINES                               
         CH    R0,=H'1'            IF JUST FINISHED FOURTH(LAST) FORM           
         BNE   *+8                                                              
         BAS   RE,NEWPAGE          SKIP TO NEXT PAGE                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT5   NTR1                                                                   
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPLAT2   NTR1                                                                   
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         CLI   RECNUM,WS           IF PRINTING TO SPOOL TAPE                    
         BNE   SPLATW2                                                          
         OC    MYP,MYSPACES        MAKE SURE NO BINARY ZEROS                    
         OC    MYP2,MYSPACES                                                    
         OC    MYP3,MYSPACES                                                    
         OC    MYP4,MYSPACES                                                    
         SPACE 1                                                                
SPLATW2  MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
NEWPAGE  NTR1                                                                   
         L     R5,ASPOOLD          ALWAYS PRINT LINE UP PAGE                    
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
ADDTOT   NTR1                      ADD TO TOTAL                                 
         L     R1,SORTREXP                                                      
         A     R1,SORTFERN                                                      
         A     R1,SORTTXRE         TAXABLE REIMBURSEMENTS                       
         LTR   R1,R1                                                            
         BZ    ADDTOTX                                                          
         CVD   R1,DUB                                                           
         AP    TOTWAGES,DUB                                                     
*                                                                               
         L     R1,SORTFTAX                                                      
         CVD   R1,DUB                                                           
         AP    TOTFTAX,DUB                                                      
*                                                                               
         L     R1,FICAWAGE                                                      
         CVD   R1,DUB                                                           
         AP    TOTFIWG,DUB                                                      
*                                                                               
         L     R1,SORTFICA                                                      
         CVD   R1,DUB                                                           
         AP    TOTFITAX,DUB                                                     
*                                                                               
         L     R1,MEDWAGE                                                       
         CVD   R1,DUB                                                           
         AP    TOTMWAGE,DUB                                                     
*                                                                               
         L     R1,MEDTAX                                                        
         CVD   R1,DUB                                                           
         AP    TOTMTAX,DUB                                                      
*                                                                               
         L     R1,SORTREXP                                                      
         CVD   R1,DUB                                                           
         AP    TOTREXP,DUB                                                      
ADDTOTX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS TOTALS AND NEGATIVE SSN FOR TOTALS OPTION         
         SPACE 2                                                                
PRNTTOT  NTR1                                                                   
         MVC   MYP(31),=CL31'W2 RECORDS READ'                                   
         EDIT  W2COUNT,(12,MYP+35),ZERO=BLANK                                   
         BAS   RE,SPLAT                                                         
         MVC   MYP(31),=CL31'W2 FORMS PRINTED'                                  
         EDIT  W2RCOUNT,(12,MYP+35),ZERO=BLANK                                  
         BAS   RE,SPLAT                                                         
         CLI   CDOPT,C'Y'                                                       
         BE    XIT                                                              
*                                                                               
         MVC   MYP(31),=CL31'NEGATIVE W2 FORMS SKIPPED'                         
         EDIT  NEGCOUNT,(12,MYP+35),ZERO=BLANK                                  
         BAS   RE,SPLAT                                                         
         OC    NEGCOUNT,NEGCOUNT                                                
         BZ    *+8                                                              
         BAS   RE,PRNTNEG          PRINT NEGATIVE SSN'S                         
         LA    R0,TOTNAMTS                                                      
         LA    R2,TOTS                                                          
PRNTTOT5 MVC   MYP(31),8(R2)                                                    
         EDIT  (P8,0(R2)),(12,MYP+35),2,ZERO=BLANK                              
         BAS   RE,SPLAT                                                         
         LA    R2,L'TOTS(R2)                                                    
         BCT   R0,PRNTTOT5                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT NEGATIVE SSN'S ONE PER LINE                     
         SPACE 1                                                                
PRNTNEG  NTR1                                                                   
         LA    R0,MAXNEG                                                        
         LA    R2,NEGTAB                                                        
PRNTNEG5 OC    0(9,R2),0(R2)                                                    
         BZ    XIT                                                              
         MVC   MYP(9),0(R2)                                                     
         BAS   RE,SPLAT                                                         
         LA    R2,9(R2)                                                         
         BCT   R0,PRNTNEG5                                                      
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
TRACOUNT DC    PL6'0'                                                           
W2COUNT  DC    PL6'0'                                                           
W2RCOUNT DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
*                                                                               
         DS    0D                                                               
TOTS     DS    0CL39               TOTALS                                       
TOTWAGES DC    PL8'0',CL31'WAGES, TIPS, OTHER COMPENSATION'                     
TOTFTAX  DC    PL8'0',CL31'FEDERAL INCOME TAX WITHHELD'                         
TOTFIWG  DC    PL8'0',CL31'SOCIAL SECURITY WAGES'                               
TOTFITAX DC    PL8'0',CL31'SOCIAL SECURITY TAX WITHHELD'                        
TOTMWAGE DC    PL8'0',CL31'MEDICARE WAGES AND TIPS'                             
TOTMTAX  DC    PL8'0',CL31'MEDICARE TAX WITHHELD'                               
TOTREXP  DC    PL8'0',CL31'BENEFITS INCLUDED IN BOX 1'                          
TOTNAMTS EQU   (*-TOTS)/L'TOTS                                                  
*                                                                               
THSCOUNT DC    PL8'0'                                                           
TAPCOUNT DC    PL6'0'                                                           
REPCOUNT DC    PL6'0'                                                           
NEGCOUNT DC    PL6'0'                                                           
TPCH     DC    H'1878'                                                          
EXES     DC    120C'X'                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
         SPACE 3                                                                
         ENTRY W2TAPE                                                           
         SPACE 1                                                                
W2TAPE   DCB   DDNAME=W2TAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=700,BUFNO=2,BLKSIZE=7000                          
         SPACE 3                                                                
*              SUBSIDIARY ROUTINES - STATE AND LOCAL LIST                       
         SPACE 1                                                                
BUILDSTA NTR1  BASE=*,LABEL=*                                                   
         XC    UNITLIST,UNITLIST   RETURN VALID STATES IN UNITLIST              
         SPACE 1                                                                
         LA    R2,UNITLIST                                                      
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
BSTA2    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'FD '                                                 
         BE    BSTA2                                                            
         CLC   TAW2UNIT,=C'CN '                                                 
         BE    BSTA2                                                            
         OC    TAW2AMTS,TAW2AMTS   IF NO AMOUNTS                                
         BZ    BSTA2               SKIP STATE                                   
         CLI   TAW2UNIT+2,C' '                                                  
         BH    BSTA2                                                            
         CLI   TIFUNIT,0                                                        
         BE    *+14                                                             
         CLC   TIFUNIT,TAW2UNIT                                                 
         BNE   BSTA2                                                            
         MVC   0(3,R2),TAW2UNIT                                                 
         LA    R2,6(R2)                                                         
         B     BSTA2                                                            
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
ADDLOCAL NTR1  BASE=*,LABEL=*                                                   
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
ALOC2    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLI   TAW2UNIT+2,C' '     IGNORE IF NOT A CITY                         
         BNH   ALOC2                                                            
         CLC   =C'LAX',TAW2UNIT    IGNORE IF LAX (FAKE CA CITY)                 
         BE    ALOC2                                                            
         OC    FILTCITY,FILTCITY   IF CITY FILTER DEFINED                       
         BZ    *+14                                                             
         CLC   TAW2UNIT,FILTCITY   FILTER ON IT                                 
         BNE   ALOC2                                                            
                                                                                
         GOTO1 TAXVAL,DMCB,(3,TAW2UNIT)  ELSE LOOK UP CORRES. STATE             
                                                                                
         LA    R2,UNITLIST         LOOK FOR MATCHING STATE FOR LOCAL            
ALOC4    CLI   0(R2),0                                                          
         BNE   ALOC5               (NOT FOUND - IGNORE)                         
                                                                                
         CLC   =C'NYC',TAW2UNIT    NYC AND PHL CAN BE TAXED                     
         BE    ALOC6                 OUTSIDE OF STATE                           
         CLC   =C'PHL',TAW2UNIT                                                 
         BE    ALOC6                                                            
         B     ALOC2               (NOT FOUND - IGNORE)                         
                                                                                
ALOC5    CLC   0(3,R2),TGTASTCY                                                 
         BE    ALOC6                                                            
         LA    R2,6(R2)                                                         
         B     ALOC4                                                            
         SPACE 1                                                                
ALOC6    CLC   3(3,R2),MYSPACES    SEE IF THERE IS A LOCAL ALREADY              
         BNH   ALOC10                                                           
                                                                                
         LR    R3,R2                                                            
         AHI   R2,6                                                             
ALOC7    AHI   R3,6                                                             
         CLI   0(R3),0             FIND EMPTY ENTRY                             
         BNE   ALOC7                                                            
         CR    R3,R2                                                            
         BE    ALOC9                                                            
ALOC8    LR    RF,R3                                                            
         AHI   RF,-6                                                            
         MVC   0(6,R3),0(RF)       MOVE PREVIOUS ENTRY FORWARD                  
         AHI   R3,-6                                                            
         CR    R3,R2                                                            
         BH    ALOC8                                                            
                                                                                
ALOC9    MVC   0(3,R2),TGTASTCY    BUILD NEW ENTRY                              
ALOC10   MVC   3(3,R2),TAW2UNIT    ADD LOCAL TO UNITLIST AFTER STATE            
         B     ALOC2                                                            
         LTORG                                                                  
         EJECT                                                                  
*              TASYSCATS BELOW                                                  
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSCATS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
MYASS    DS    A                                                                
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
SKIPSW   DS    XL1                                                              
LUPSW    DS    CL1                                                              
MYSORTER DS    A                                                                
MYAREMOT DS    A                                                                
DYNALLOC DS    A                                                                
FICAWAGE DS    F                                                                
MEDWAGE  DS    F                                                                
MEDTAX   DS    F                                                                
         SPACE 1                                                                
LASTSSN  DS    CL9                 LAST SSN                                     
LASTSTAT DS    CL3                 LAST STATE PRINTED                           
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 Y=TRACE                                      
WRITOPT  DS    CL1                 Y=WRITE BACK                                 
TAPEOPT  DS    CL1                 Y=WRITE TAPE                                 
ALLOPT   DS    CL1                 ALL OPTION                                   
TOTOPT   DS    CL1                 Y=TOTALS ONLY                                
SKIPOPT  DS    CL1                 Y=PRINT 1 W2 PER PAGE                        
CDOPT    DS    CL1                 Y=SORT LIKE W2SPOOL, FOR CD BURNING          
NYPHLOPT DS    CL1                 Y=NYC/PHL LOCALS                             
         DS    CL2                 SPARE                                        
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
YEAR     DS    CL4                 CCYY                                         
FILTCITY DS    CL3                 FILTER CITY                                  
         SPACE 1                                                                
ATHISEX  DS    A                                                                
EMPNAME  DS    CL36                                                             
EMPADD   DS    CL120                                                            
EMPFDID  DS    CL14                                                             
ATHISW4  DS    A                                                                
W4NAME   DS    CL36                                                             
W4ADD    DS    CL(4*39)                                                         
SAVEEL   DS    XL1                                                              
W4TYPE   DS    XL1                                                              
THISUNIT DS    CL3                                                              
UNITLIST DS    CL240                                                            
MYRERUN  DS    CL1                                                              
MYBYTE   DS    CL1                 X'80' = WROTE BACK RECORD ALREADY            
*                                  X'40' = PRINT PENDING                        
NEGTAB   DS    CL(MAXNEG*9)        TABLE FOR NEGATIVE SSN'S                     
SSNSTART DS    CL9                 STARTING SSN                                 
MAXNEG   EQU   30                                                               
*                                                                               
         DS    0D                                                               
         EJECT                                                                  
*              SORT RECORD AREA                                                 
         SPACE 3                                                                
         DS    0D                                                               
SORTIO   DS    0CL84                                                            
SORTKEY  DS    0C                                                               
SORTKEMP DS    CL3                                                              
SORTKUNT DS    CL3                                                              
SORTKSSN DS    CL9                                                              
         ORG   SORTKUNT            FOR WS REPORT                                
SORTK2SN DS    CL9                 SORT BY SSN THEN STATE                       
SORTK2UN DS    CL3                                                              
         ORG                                                                    
         DS    CL5                                                              
SORTSSN  DS    CL9                                                              
SORTSTAT DS    CL3                                                              
SORTLOCL DS    CL3                                                              
SORTPRNT DS    CL1                                                              
SORTFERN DS    F                                                                
SORTFTAX DS    F                                                                
SORTSERN DS    F                                                                
SORTSTAX DS    F                                                                
SORTLERN DS    F                                                                
SORTLTAX DS    F                                                                
SORTFICA DS    F                                                                
SORTREXP DS    F                                                                
SORTSDI  DS    F                                                                
SORTSUI  DS    F                                                                
SORTSFLI DS    F                                                                
SORTNHA  DS    F                                                                
SORTTXRE DS    F                                                                
SORTSTXR DS    F                                                                
SORTLTXR DS    F                                                                
SORTNAMT EQU   (*-SORTFERN)/L'SORTFERN                                          
SORTLNQ  EQU   *-SORTKEY                                                        
         EJECT                                                                  
*              TAPE RECORD AREA                                                 
         SPACE 3                                                                
         ORG   SORTIO+1000                                                      
         DS    0D                                                               
PRETAPIO DS    F                                                                
TAPEIO   DS    1000C                                                            
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
MYPRINT  DSECT                                                                  
         DS    CL132                                                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES TO SPOOL TAPE                         
         SPACE                                                                  
PSPLD    DSECT                                                                  
         DS    CL80                LINE 1                                       
PSFEARN  DS    CL14                WAGES, TIPS, OTHER COMPENSATION              
         DS    CL9                                                              
PSFTAX   DS    CL14                FEDERAL INCOME TAX WITHHELD                  
*                                                                               
         ORG   PSPLD               LINE 3                                       
         DS    CL80                                                             
PSFWAGE  DS    CL14                SOCIAL SECURITY WAGES                        
         DS    CL9                                                              
PSFICA   DS    CL14                SOCIAL SECURITY TAX WITHHELD                 
*                                                                               
         ORG   PSPLD               LINE 4                                       
         DS    CL10                                                             
PSEMPNM  DS    CL30                EMPLOYER'S NAME                              
*                                                                               
         ORG   PSPLD               LINE 5                                       
         DS    CL10                                                             
PSEADD   DS    CL30                EMPLOYER'S ADDRESS LINE 1                    
         DS    CL40                                                             
PSMWAGE  DS    CL14                MEDICARE WAGES                               
         DS    CL9                                                              
PSMTAX   DS    CL14                MEDICARE TAX WITHHELD                        
*                                                                               
         ORG   PSPLD               LINE 6                                       
         DS    CL10                                                             
PSEADD2  DS    CL30                EMPLOYER'S ADDRESS LINE 2                    
*                                                                               
         ORG   PSPLD               LINE 7                                       
         DS    CL10                                                             
PSEADD3  DS    CL30                EMPLOYER'S ADDRESS LINE 3                    
         DS    CL63                                                             
PSBENFIT DS    CL14                BENEFITS INCLUDED IN BOX 1                   
*                                                                               
         ORG   PSPLD               LINE 9                                       
         DS    CL103                                                            
PSOTH    DS    CL14                SDI                                          
*                                                                               
         ORG   PSPLD               LINE 10                                      
         DS    CL10                                                             
PSW4NAME DS    CL30                EMPLOYEE'S NAME                              
         DS    CL63                                                             
PSOTH2   DS    CL14                SUI                                          
*                                                                               
         ORG   PSPLD               LINE 11                                      
         DS    CL10                                                             
PSW4ADD  DS    CL30                EMPLOYEE'S ADDRESS - LINE 1                  
         DS    CL18                                                             
PSEMPID  DS    CL14                EMPLOYER'S FEDERAL IDENT. NUMBER             
         DS    CL8                                                              
PSTONLY  DS    CL12                (STATE ONLY)                                 
         DS    CL11                                                             
*                                                                               
         ORG   PSPLD               LINE 12                                      
         DS    CL10                                                             
PSW4ADD2 DS    CL30                EMPLOYEE'S ADDRESS - LINE 2                  
*                                                                               
         ORG   PSPLD               LINE 13                                      
         DS    CL10                                                             
PSW4ADD3 DS    CL30                EMPLOYEE'S ADDRESS - LINE 3                  
         DS    CL18                                                             
PSW4SSN  DS    CL14                EMPLOYEE'S SOCIAL SECURITY NUMBER            
*                                                                               
         ORG   PSPLD               LINE 14                                      
         DS    CL10                                                             
PSW4ADD4 DS    CL30                EMPLOYEE'S ADDRESS - LINE 4                  
*                                                                               
         ORG   PSPLD               LINE 15                                      
         DS    CL68                                                             
PSPP     DS    CL1                 PENSION PLAN CHECK OFF BOX                   
*                                                                               
         ORG   PSPLD               LINE 17                                      
         DS    CL4                                                              
PSSTATE  DS    CL3                 STATE                                        
         DS    CL4                                                              
PSSID    DS    CL11                EMPLOYR'S STATE I.D. NUMBER                  
         DS    CL12                                                             
PSSEARN  DS    CL13                STATE WAGES, TIPS, ETC                       
         DS    CL5                                                              
PSSTAX   DS    CL11                STATE INCOME TAXES                           
         DS    CL4                                                              
PSLOCLNM DS    CL8                 LOCALITY NAME                                
         DS    CL14                                                             
PSLEARN  DS    CL10                LOCAL WAGES,TIPS, ETC                        
         DS    CL9                                                              
PSLTAX   DS    CL9                 LOCAL INCOME TAX                             
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORDS                                      
         SPACE 3                                                                
TAPED    DSECT                                                                  
TAPEKEY  DS    0CL20               KEY LENGTH OF 20                             
TAPEREC  DS    0C                  RECORD LENGTH OF 700                         
         SPACE 1                                                                
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*DDMASTD                                                                        
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPECD                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113TAREP2C   05/09/15'                                      
         END                                                                    
