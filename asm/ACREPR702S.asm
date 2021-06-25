*          DATA SET ACREPR702S AT LEVEL 096 AS OF 05/01/02                      
*PHASE ACR702A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACLIST                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'CHARGEABLE TIME ANALYSIS'                                       
*-------------------------------------------------------------------*           
*        REQUESTED OFF THE 1R LEDGER                                *           
*                                                                   *           
*        READS 1R ACCOUNT RECORD - GETS NAME                        *           
*        READS 1R TRANSACTIONS -                                    *           
*                                                                               
*             X'40' - HOURS         -  ACPSHOUR                     *           
*                   - BILLING RATE  -  ACPSRATE                     *           
*                   - TYPE OF TIME  -  ACPSSTAT                     *           
*                                                                               
*             X'50' - 'N' TIME HOURS                                            
*                                                                               
*             X'44' - TIMESHEET DATE - TRNSDATE                                 
*                   - TRANSACTION OFFICE - TRNSOFFC                             
*                                                                               
*             X'51' - IF LEN = X'11' USE CLINT/PROD  - ACPCCLI      *           
*                     IF LEN = X'22' USE CL/PR/JOB   - ACPCPRJ      *           
*                                                                               
*        START END MONTHS - MMM/YY  MMM/YY                          *           
*                           READ BY 1R                              *           
*                                                                               
*        MOA RANGE          MMM/YY       TIME  FOR MMM/YY ONLY                  
*                           -MMM/YY      TIME  THRU MMM/YY                      
*                           MMM/YY-MMM/YY  MMM/YY THRU MMM/YY                   
*                           BLANK        TIME  ETERNITY                         
*                           READ BY TIME BILLED.                    *           
*                                                                   *           
*        OPTION 1           TYPE OF TIME                                        
*                           B- B TIME                                           
*                           R- R TIME                                           
*                           N- N TIME                                           
*                           A- ALL TIME                                         
*                           C- B AND R TIME            *                        
*                           BLANK- B+R TIME                                     
*                                                                               
*        OPTION 2           SJ LEVEL OF DETAIL                                  
*                           N- NO SJ DATA ON REPORT                 *           
*                           C- CLIENT LEVEL                         *           
*                           P-CLI,PRODUCT                           *           
*                           J-CLI,PRO,JOB               *           *           
*                                                                   *           
*        OPTION 3           TASK                                                
*                            N- NO TASK                                         
*                            E- EMPLOYEE/TASK                                   
*                            T- TASK EMPLOYEE                                   
*        NOTE: TASK IS INDEPENDENT OF EMPLOYEE                                  
*                                                                   *           
*        OPTION 4           1R LEVEL OF DETAIL                                  
*                           1- LEVEL 1 ONLY                         *           
*                           2- LEVEL 1 AND 2                        *           
*                           3- LEVEL 1,2,3                          *           
*                           4- LEVEL 1,2,3,4             *                      
*                           A- LEVEL 1 AND 4                        *           
*                           B- LEVEL 1, 2 AND 4                     *           
*                           C- LEVEL 3 AND 4                        *           
*                           D- LEVEL 4 ONLY                                     
*        NOTE: WHEN HIGHER LEVELS OF 1R ARE SUPRESSED, THE 12                   
*              CHARACTER ACCOUNT PRINTS AT LEVLE 4.                             
*                                                                   *           
*        OPTION 5           SORT ORDER                                          
*                           S-SJ/1R                                             
*                           1-1R/SJ                                 *           
*                                                                   *           
*        OPTION 6           BUCKET TYPES                                        
*                           BLANK- CUR HRS, CUR REV, YTD HRS YTD REV            
*                           H- B TIME, R TIME, N TIME, ALL TIME                 
*                           T- TIME SHEET DATA                                  
*                           A- AGEING                                           
*                           B- B HRS, B REV, R HRS, R REV                       
*                                                                               
*        PROFILE OPTIONS                                                        
*                                                                               
*        REQUEST OPTIONS 1-6 HAVE CORRESPONDING PROFILE OPTIONS                 
*              ADDITIONAL PROFILE OPTIONS ARE:                                  
*                                                                               
*        - BREAK BY SERVICE GROUP- Y,N                                          
*              MUST BE ON AT THE LEDGER LEVEL AND ON AT THE                     
*              1R LEVEL 1 ACCOUNT WHOSE SERVICE GROUP THE USER                  
*              WANTS TO BREAK BY SERVICE GROUP                                  
*                                                                               
*        - PRINT OFFICES- Y,N                                                   
*              IF THE USER HAS REQUESTED AT LEAST 1 LEVEL OF SJ DATA            
*              THE REPORT WILL SORT THE SJ ACCOUNTS INTO OFFICE                 
*              ORDER, USING OFFICE GROUPS WHERE APPLICABLE                      
*                                                                               
*        - PRINT TOTALS- Y,N      (DEFAULT-N)                                   
*              PRINT TOTALS AT ALL LEVELS, IF YOU NEED THEM OR NOT              
*                                                                               
*--------------------------------------------------------------------*          
* PROGRAM DUMPS ARE CAUSED BY THE FOLLOWING:                        *           
*  1) NO NAME ELEMENT                                               *           
*  2) TABLE FULL                                                                
*  3) BAD READ                                                                  
*--------------------------------------------------------------------*          
*        HISTORY                                                                
*--------------------------------------------------------------------*          
* 10/13/94  SOFT 1R IN THE NEWACC ROUTINE                                       
*--------------------------------------------------------------------*          
         EJECT                                                                  
ACR702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,--ACR7--,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         USING ACR7D,RC                                                         
         LA    RC,SPACEND                                                       
*------------------------                                                       
* ROUTINE FOR RUN FIRST *                                                       
*------------------------                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
*                                                                               
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         XC    ABUFF,ABUFF                                                      
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------*                              
*              ROUTINE FOR REQUEST FIRST         *                              
*------------------------------------------------*                              
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   WRKCODE                                                          
*                                                                               
         BAS   RE,GETBUFF                                                       
         BAS   RE,GETOFF                                                        
*                                                                               
         MVC   NAMETEMP,NAMENEXT   SAVE END OF OFFICE NAMES                     
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FIRSTFUT,C'Y'                                                    
*                                       -- INIT FOR SORT --                     
         XC    ALSORT,ALSORT                 CLEAR A(LAST SORT)                 
         LA    R1,SRTKEYLN                   SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         L     RF,=A(SORTCARD)                                                  
         UNPK  15(3,RF),DUB+6(2)                                                
*                                                                               
         LA    R1,SRTLNQ                     SORT RECORD LENGTH                 
         CVD   R1,DUB                        CONVERT REC LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         L     RF,=A(RECCARD)                                                   
         UNPK  22(3,RF),DUB+6(2)                                                
         GOTO1 SORTER,DMCB,A(SORTCARD),A(RECCARD),0                             
*                                                                               
*                                       -- PACK PERIOD DATES --                 
         XC    STRDATE,STRDATE                                                  
         MVC   ENDDATE,=X'FFFFFF'                                               
         MVC   TDATEP,SPACES                                                    
         MVC   LISTDATA(20),SPACES  SPACE TO SAVE PRINTABLE DATES               
         XC    MYBYTE,MYBYTE                                                    
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRDATE) START=YMD PACKED              
         GOTO1 DATCON,DMCB,(0,QSTART),(5,LISTDATA) MMMDD/YY                     
         OI    MYBYTE,GOTSTDT                                                   
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDATE) ENDATE=YMD PACKED               
         GOTO1 DATCON,DMCB,(0,QEND),(5,LISTDATA+10) MMMDD/YY                    
         OI    MYBYTE,GOTENDT                                                   
*                                                                               
REQF20   OC    MYBYTE,MYBYTE       ANY START OR END DATE                        
         BZ    REQF20F             NO                                           
         TM    MYBYTE,GOTSTDT+GOTENDT   BOTH START AND END                      
         BNO   REQF20A                  NO                                      
*                                                                               
         MVC   TDATEP(8),LISTDATA  START DATE                                   
         MVC   TDATEP+9(2),=C'TO'  TO                                           
         MVC   TDATEP+12(8),LISTDATA+10 ENDDATE                                 
         B     REQF20F                                                          
*                                                                               
REQF20A  TM    MYBYTE,GOTENDT     ENDATE                                        
         BNO   REQF20B             NO, MUST BE START                            
         MVC   TDATEP+3(3),=C'END'                                              
         MVC   TDATEP+8(8),LISTDATA+10                                          
         B     REQF20F                                                          
*                                                                               
REQF20B  MVC   TDATEP+3(5),=C'START'  STARTDATE                                 
         MVC   TDATEP+9(8),LISTDATA                                             
         B     REQF20F                                                          
*                                                                               
         USING ACMD,R2                                                          
REQF20F  L     R2,AMONACC                                                       
         CLI   ACMMEND,X'FF'          IS THERE A MOS END DATE?                  
         BE    REQF30              NO, SET DATES                                
*                                                                               
         MVC   MYMEND(2),ACMMEND                                                
         MVC   MYMSTR(2),ACMFDTE     FISCAL START DATE (FROM MONACC)            
         OC    ACMMSTR,ACMMSTR       DO THEY WANT A MOS START                   
         BZ    *+10                NO                                           
         MVC   MYMSTR(2),ACMMSTR    YES, THIS OVERRIDES FDATE                   
         B     REQF40                                                           
*                                                                               
REQF30   GOTO1 DATCON,DMCB,(4,RCDATE),(1,WORK) PACK TODAY                       
         MVC   MYMSTR,WORK                                                      
         MVI   MYMSTR+1,1          JAN THIS YEAR IS DEFAULT MOS START           
         USING ACCOMPD,R2                                                       
         L     R2,ADCMPEL                                                       
         CLI   ACMPSTM,0           DO THEY HAVE A FISCAL START MON              
         BE    REQF35              NO                                           
         ZIC   R1,ACMPSTM          YES, CONVERT IT TO NUMERIC                   
         N     R1,=F'15'           TURN OFF HI BITS                             
         TM    ACMPSTM,X'F0'       WAS IT F1-F9                                 
         BO    *+8                 YES, DONE                                    
         LA    R1,15(R1)           NO, ADD COMVERSION FACTOR                    
         STC   R1,MYMSTR+1                                                      
*                                                                               
REQF35   MVC   MYMEND,WORK         USE TODAYS YYMM AS END MOS                   
         CLC   MYMEND,MYMSTR       END GREATER THAN START ?                     
         BNL   REQF40              EQUAL IS OK ALSO                             
*                                                                               
         MVI   WORK+1,X'0F'                                                     
         SP    WORK(2),=P'10'      SUBTRACT 1 FROM YEAR                         
         MVC   MYMSTR(1),WORK                                                   
*                                                                               
REQF40   MVC   CURMONP,SPACES                                                   
*                                                                               
         MVC   CURMON,ENDDATE                                                   
         MVC   CURMONP+7(16),=CL16'TRANSACTION DATE'                            
         MVC   ACTUCUR+2(2),=S(SVDATE)                                          
         CLI   CURMON,X'FF'                                                     
         BNE   REQF50                                                           
*                                                                               
         MVC   CURMON,MYMEND                                                    
         MVC   CURMONP+7(16),=CL16'ACCOUNTING MONTH'                            
         MVC   ACTUCUR+2(2),=S(SVMOS)                                           
*                                                                               
REQF50   MVC   WORK,CURMON                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,CURMONP) FOR HEADER                      
         MVC   WORK,MYMSTR                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,FRMMONP) FOR HEADER                      
         MVC   WORK,MYMEND                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,THRMONP) FOR HEADER                      
*                                                                               
         USING RUNXTRAD,R2                                                      
         MVI   LISTSW,C'N'                                                      
         L     R2,VEXTRAS          LOOK FOR CLIENT LIST                         
         ICM   R4,15,VLISTREC                                                   
         BZ    REQF150                                                          
*                                                                               
         USING ACLISTD,R4                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BAD LIST REC                                 
         CLI   ACLITYPE,C'A'       ACCOUNT LIST?                                
         BNE   REQF150             NO FUGETABOUDIT                              
*                                                                               
         MVI   LISTSW,C'Y'         MUST CHECK LIST                              
*                                                                               
         USING ACCOMPD,R2                                                       
REQF150  L     R2,ADCMPEL                                                       
         MVI   NEWOFF,C'N'                                                      
         TM    ACMPSTA4,X'01'                                                   
         BNO   *+8                                                              
         MVI   NEWOFF,C'Y'                                                      
*                                                                               
         MVC   SVCUL(1),RCCOMPFL                                                
         MVC   SVCUL+1(2),=C'SJ'                                                
         LA    R3,REQTOTS                                                       
         BAS   RE,ZAPEM                                                         
         USING BIND,R2                                                          
         L     R2,AOFFLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,ADEPLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,ASDELST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,AEMPLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,ACLILST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,APROLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,AJOBLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         L     R2,ATSKLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         MVC   NAMENEXT,NAMETEMP   START NAME TABLE AFTER OFFICES               
*                                                                               
         BAS   RE,INITTAB                                                       
         MVI   FCRDWORK,C'Y'       HAVE MONACC PASS YOU PROCWORK                
         MVI   FCRDTIME,C'Y'       AND TMS POSTINGS                             
*                                                                               
REQFX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE WORK CODE NAMES, SAVE AS TASK                                     
*----------------------------------------                                       
WRKCODE  CLI   MODE,PROCWORK       WORKCODE PASSED IN ADACC ?                   
         BNE   FRSTLEDG                                                         
*                                                                               
         USING ACANALD,R4                                                       
         MVI   ELCODE,X'12'                                                     
         L     R4,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   WRKCX                                                            
*                                                                               
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTNAME,SPACES                                                  
         MVC   LISTKEY(L'ACANCODE),ACANCODE                                     
         MVC   LISTNAME+2(L'ACANDESC),ACANDESC                                  
         MVI   LISTNAME,X'20'                                                   
         MVI   LISTNAME+1,L'ACANDESC+2                                          
         LA    R4,LISTNAME                                                      
         L     R3,ATSKLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
*        BE    WRKCX                                                            
*        MVI   P+1,C'*'                                                         
*        MVC   PSECOND+1(32),=C'**WARNING, TASK TABLE IS FULL **'               
*        MVI   PTHIRD+1,C'*'                                                    
*        BAS   RE,PRINTEM                                                       
WRKCX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE LEDGER NAMES IN TABLE                                             
*        UPDATE TABLE WITH 1R LEDGER INFO                                       
*----------------------------------------                                       
FRSTLEDG CLI   MODE,LEDGFRST       LEDGER                                       
         BNE   FRSTLEVA            NO                                           
*                                                                               
         USING ACHEIRD,R3                                                       
         USING TABLED,R2                                                        
         L     R3,ADLDGHIR                                                      
         XC    BYTE,BYTE           CLEAR BYTE                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVI   LEVEL,EOFLEV                                                     
         BAS   RE,GETLEV                                                        
         MVC   TABTITLE,ACHRDESA                                                
         MVC   MYBYTE,ACHRLEVA                                                  
         BAS   RE,SETHEIR                                                       
*                                                                               
         MVI   LEVEL,DEPLEV                                                     
         BAS   RE,GETLEV                                                        
         MVC   TABTITLE,ACHRDESB                                                
         MVC   MYBYTE,ACHRLEVB                                                  
         BAS   RE,SETHEIR                                                       
*                                                                               
         MVI   LEVEL,SDELEV                                                     
         BAS   RE,GETLEV                                                        
         MVC   TABTITLE,ACHRDESC                                                
         MVC   MYBYTE,ACHRLEVC                                                  
         BAS   RE,SETHEIR                                                       
*                                                                               
         MVI   LEVEL,EMPLEV                                                     
         BAS   RE,GETLEV                                                        
         MVC   TABTITLE,ACHRDESD                                                
         MVC   MYBYTE,ACHRLEVD                                                  
         BAS   RE,SETHEIR                                                       
*                                                                               
         MVI   LEVEL,EMP12LEV                                                   
         BAS   RE,GETLEV                                                        
         MVC   TABTITLE,ACHRDESD                                                
         MVC   TABLN,BYTE                                                       
         XC    SVGLIST,SVGLIST     LIST OF SVGP OFFICES, BUILT AT LEVA          
         SR    R3,R3               SET FOR LEDGER CALL OF BUILDPRO              
         GOTO1 =A(BUILDPRO),DMCB,(RC)                                           
         MVC   OPTIONS,SPACES      CLEAR OPTIONS FOR FITST SETORD CALL          
         MVI   OPTSJ,C'J'                                                       
         MVI   OPT1R,C'4'                                                       
         MVI   OPTSRT,C'S'                                                      
         MVI   OPTTIME,C'C'                                                     
         MVI   MASK,X'FF'                                                       
         MVI   REPSTAT,CURRHRS                                                  
         BAS   RE,SETORD           SET SORT ORDER BASED ON REQUEST              
*                                                                               
         MVI   BYTE,C'C'           DEFAULT IS B AND R TIME                      
         CLI   OPTTIME,C' '                                                     
         BNH   *+10                                                             
         MVC   BYTE,OPTTIME        TIME TYPE PROFILE OPTION                     
         XC    TIMETYPE,TIMETYPE                                                
         CLI   BYTE,C'N'           N TIME ONLY?                                 
         BNE   *+8                 NO                                           
         OI    TIMETYPE,ACPSNOT                                                 
         CLI   BYTE,C'B'           B TIME ONLY                                  
         BNE   *+8                 NO                                           
         OI    TIMETYPE,ACPSBIL                                                 
         CLI   BYTE,C'R'           R TIME ONLY                                  
         BNE   *+8                 NO                                           
         OI    TIMETYPE,ACPSRTE                                                 
         CLI   BYTE,C'C'           B+R TIME?                                    
         BNE   *+8                 NO                                           
         OI    TIMETYPE,ACPSRTE+ACPSBIL                                         
         CLI   BYTE,C'A'           ALL TIME?                                    
         BNE   *+8                 NO                                           
         OI    TIMETYPE,ACPSRTE+ACPSBIL+ACPSNOT                                 
*                                                                               
         MVI   NUMCOLS,4           ASSUME 4 COLS                                
         CLI   OPTBUCK,C'H'        KEEP TIME TYPE BUCKETS                       
         BNE   FRLDG50             NO                                           
*                                                                               
         MVI   RCSUBPRG,3                                                       
*                                                                               
FRLDG50  CLI   OPTBUCK,C'B'        B HR, REV, R HR REV                          
         BNE   FRLDG60             NO                                           
*                                                                               
         MVI   RCSUBPRG,4                                                       
         MVI   NUMCOLS,6           NEED 6 COLS FOR THIS                         
*                                                                               
FRLDG60  CLI   OPTBUCK,C'A'        AGEING HEADERS                               
         BNE   FRLDGX              NO                                           
*                                                                               
         MVI   RCSUBPRG,5                                                       
         MVI   NUMCOLS,5           NEED 5 COLS FOR THIS                         
         GOTO1 =A(AGEHEADS),DMCB,(RC)                                           
*                                                                               
FRLDGX   GOTO1 =A(SETLEN),DMCB,(RC)                                             
         B     XIT                                                              
*                                                                               
SETHEIR  MVC   TABDLEN,MYBYTE                                                   
         IC    R0,BYTE                                                          
         IC    R1,MYBYTE                                                        
         SR    R1,R0                                                            
         STC   R1,TABLN                                                         
         MVC   BYTE,MYBYTE                                                      
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE 1R OFFICE CODE/NAME                                               
*----------------------------------------                                       
FRSTLEVA CLI   MODE,LEVAFRST       1R OFFICE                                    
         BNE   FRSTLEVB            NO                                           
*                                                                               
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRA                                                       
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVANAM                                                      
         L     R3,AOFFLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         CLI   OPTSVGP,C'Y'        BREAK ON SERVICE GROUP                       
         BNE   FLVAX               NO,                                          
         L     R3,ADHEIRA                                                       
         LA    R3,3(R3)            POINT TO ACCOUNT CODDE                       
         GOTO1 =A(BUILDPRO),DMCB,(RC)                                           
         BZ    FLVAX               ACCOUNT PROFILE NOT FOUND                    
         CLI   PROSVGP,C'Y'        BREAK THIS OFF GROUP ON SERVICE GRP          
         BNE   FLVAX               NO,                                          
         LA    R2,SVGLIST          ADD OFFICE GROUP TO LIST                     
         LA    R0,L'SVGLIST                                                     
FLVA40   CLI   0(R2),0             FIND EMPTY SPACE IN TABLE                    
         BE    FLVA50                                                           
         LA    R2,1(R1)                                                         
         BCT   R0,FLVA40                                                        
         DC    H'0'                                                             
*                                                                               
FLVA50   MVC   0(1,R2),0(R3)                                                    
*                                                                               
FLVAX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE 1R DEPARTMENT NAME                                                
*----------------------------------------                                       
FRSTLEVB CLI   MODE,LEVBFRST       1R DEP                                       
         BNE   FRSTLEVC            NO                                           
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRB                                                       
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVBNAM                                                      
         L     R3,ADEPLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*----------------------------------------                                       
*        SAVE 1R SUB DEPARTMENT NAME                                            
*----------------------------------------                                       
FRSTLEVC CLI   MODE,LEVCFRST       1R SUB DEP                                   
         BNE   PROCAC              NO                                           
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRC                                                       
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVCNAM                                                      
         L     R3,ASDELST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
PROCAC   CLI   MODE,PROCACC           IS IT A TRANSACTION?                      
         BNE   ACTUAL                 NO - GET NEXT RECORD                      
         XC    ACCSTAT,ACCSTAT                                                  
         EJECT                                                                  
*----------------------------------------                                       
*  GET DATA FROM TRANSACTION            *                                       
*  OFFICE FROM 44 EL                    *                                       
*  RATE AND HOURS FROM 40 EL            *                                       
*  CLI, PRO AND TASK FROM 50 EL         *                                       
*  IF TRNSDATE < STRDATE OR > END REJECT *                                      
*  IF TRNSDATE > STRDATE OR < END,      *                                       
*  USE FOR YTD FIGURES                  *                                       
*  IF JOB IS FOUND ON THE 51 PUT OUT A  *                                       
*  JOB RECORD W/ WORK DATE AND TASK     *                                       
*----------------------------------------                                       
*                                                                               
ACTUAL   CLI   MODE,PROCTRNS          IS IT A TRANSACTION?                      
         BNE   REQLST                 NO - GET NEXT RECORD                      
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'        THIS IS A 44 EL?                             
         BNE   XIT                 NO, BAD TRANS                                
         CLI   TRNSTYPE,49            IS TRANS TYPE 49?                         
         BNE   XIT                    NO - SKIP                                 
         CLC   TRNSDATE,STRDATE    DATE FILTERING                               
         BL    XIT                                                              
         CLC   TRNSDATE,ENDDATE                                                 
         BH    XIT                                                              
*                                                                               
         TM    TRNSSTAT,X'20'      IS THIS A REVERSAL?                          
         BO    XIT                 YES                                          
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         CLC   ACMMDTE,MYMSTR                                                   
         BL    XIT                                                              
         CLC   ACMMDTE,MYMEND                                                   
         BH    XIT                                                              
         BAS   RE,OFFCHK                                                        
         BNE   XIT                                                              
*                                                                               
*                                                                               
         TM    ACCSTAT,GOTACC                                                   
         BO    ACTU90                                                           
         MVI   ACCSTAT,GOTACC                                                   
         BAS   RE,NEWACC                                                        
*                                                                               
*                                                                               
ACTU90   MVC   SVOFFS(2),TRNSOFFC     TRANSACTION OFFICE                        
         CLC   QOFFICE,SPACES                                                   
         BE    ACTU110                                                          
         L     R2,ADOFFLST                                                      
         BCTR  R2,0                NUMBER IN LIST                               
         ZIC   R0,0(R2)            IS BYTE BEFORE LIST                          
         LTR   R0,R0               ANY ACCES                                    
         BZ    XIT                 NO                                           
         LA    R2,1(R2)                                                         
ACTU100  CLC   TRNSOFFC(2),0(R2)                                                
         BE    ACTU110                                                          
         LA    R2,2(R2)                                                         
         BCT   R0,ACTU100                                                       
         B     XIT                 NOT FOUND                                    
*                                                                               
         USING BIND,R2                                                          
ACTU110  CLI   OFFSTAT,C'G'        GET OFFICE GROUP OF THIS OFFICE              
         BNE   ACTU135             IF THEY USE THEM                             
         MVC   SVOFG,SPACES        SAVE THE OFFICE GROUP                        
         L     R2,AOFFLSTS                                                      
         L     R0,BININ                                                         
         LA    R2,BINTABLE                                                      
ACTU120  CLC   0(2,R2),SVOFFS                                                   
         BE    ACTU130                                                          
         LA    R2,NMPTDLN(R2)                                                   
         BCT   R0,ACTU120                                                       
*                                                                               
         USING NMPTD,R2                                                         
ACTU130  ICM   R2,15,NMPTPTR       GET THE 20 EL FOR THIS OFFICE                
         MVC   SVOFG(1),2(R2)      SAVE THE OFFICE GROUP                        
         MVC   SVSVG(1),SVOFFS                                                  
*                                                                               
         USING ACMD,R2                                                          
ACTU135  L     R2,AMONACC             DATE PACKED YMD                           
         MVC   SVMOS,ACMMDTE                                                    
         DROP  R2                                                               
         MVC   SVDATE,TRNSDATE                                                  
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BE    ACTU138                                                          
*                                                                               
         USING TRCASHD,R4                                                       
         TM    TIMETYPE,ACPSNOT    NO 40 EL, DO THEY WANT N TIME                
         BZ    XIT                 NO                                           
*                                                                               
         L     R4,ADTRANS          CHECK FOR N TIME                             
         MVI   ELCODE,X'50'                                                     
ACTU136  BAS   RE,NEXTEL                                                        
         BNE   XIT                 NOT N TIME                                   
*                                                                               
         CLI   TRCSTYPE,C'H'       HOURS HERE                                   
         BNE   ACTU136             MAYBE THERE IS ANOTHER 50 EL                 
*                                                                               
         ZAP   TRNHOUR,TRCSAMNT    SAVE TRANSACTION AMOUNTS                     
         ZAP   TRNRATE,=P'0'                                                    
         ZAP   TRNAMNT,=P'0'                                                    
         MVI   SVSTAT,ACPSNOT                                                   
         B     ACTU139             FLAG AS N TIME                               
*                                                                               
         USING ACPERSD,R4                                                       
ACTU138  CLI   ACPSLEN,ACPSSTAT-ACPERSD IS TYPE OF TIME DEFINED IN THIS         
         BNH   XIT                      40 EL, NO                               
*                                                                               
*                                                                               
         MVC   BYTE,TIMETYPE       SEE IF I WANT THIS TYPE OF TIME              
         NC    BYTE,ACPSSTAT                                                    
         BZ    XIT                 I DON'T                                      
*                                                                               
         ZAP   TRNRATE,ACPSRATE    SAVE TRANSACTION AMOUNTS                     
         ZAP   TRNHOUR,ACPSHOUR                                                 
         ZAP   PL16,TRNRATE      CALCULATE BILLABLE AMOUNT                      
         MP    PL16,TRNHOUR                                                     
         SRP   PL16,64-2,5                                                      
         ZAP   TRNAMNT,PL16                                                     
         MVC   SVSTAT,ACPSSTAT                                                  
*                                                                               
ACTU139  LA    R1,SVBUCKS                                                       
         LA    R0,SVNUMBUK                                                      
ACTU140  ZAP   0(BUCKLN,R1),=P'0'            CLEAR TO PACKED ZEROS              
         LA    R1,BUCKLN(R1)                 BUMP TO NEXT BUCKET                
         BCT   R0,ACTU140                                                       
*                                                                               
*                                                                               
         L     R4,ADTRANS                                                       
         MVI   ELCODE,X'51'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING ACPCD,R4                                                         
         MVC   SVCLI(3),ACPCCLI+3                                               
         CLC   QSELECT,SPACES      ANY LIMIT CLIENT (OR LIST)                   
         BE    ACTU200             NO                                           
*                                                                               
         CLI   LISTSW,C'Y'         DONE WITH A LIST                             
         BE    ACTU190             YES                                          
         CLC   SVCLI(3),QSELECT                                                 
         BNE   XIT                                                              
         B     ACTU200                                                          
*                                                                               
ACTU190  L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         GOTO1 ACLIST,DMCB,VLISTREC,SVCLI                                       
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,C'E'           EXCLUDE?                                     
         BE    XIT                 YES                                          
         DROP  RF                                                               
*                                                                               
ACTU200  MVC   SVPRO(3),ACPCCLI+6                                               
         MVC   SVSJ,ACPCCLI+3                                                   
         MVC   SVJOB,SPACES                                                     
*                                                                               
         CLI   RCSUBPRG,3          SEPARATING TIME BY TYPE                      
         BNE   ACTU220            NO                                            
*                                                                               
         LA    R2,SVBTIME                                                       
         TM    SVSTAT,ACPSBIL                                                   
         BO    ACTU210                                                          
*                                                                               
         LA    R2,SVRTIME                                                       
         TM    SVSTAT,ACPSRTE                                                   
         BO    ACTU210                                                          
*                                                                               
         LA    R2,SVNTIME                                                       
         TM    SVSTAT,ACPSNOT                                                   
         BNO   ACTU250                                                          
*                                                                               
ACTU210  ZAP   0(8,R2),TRNHOUR                                                  
         ZAP   SVATIME,TRNHOUR                                                  
         B     ACTU250                                                          
*                                                                               
ACTU220  CLI   RCSUBPRG,4          B HRS, B REV, R HRS, R REV                   
         BNE   ACTU235A                                                         
         TM    SVSTAT,ACPSBIL                                                   
         BNO   ACTU230                                                          
         ZAP   SVCAMNT,TRNAMNT   SAVE BTIME HERE                                
         ZAP   SVCHR,TRNHOUR                                                    
         B     ACTU235                                                          
*                                                                               
ACTU230  TM    SVSTAT,ACPSRTE                                                   
         BNO   ACTU250                                                          
         ZAP   SVYAMNT,TRNAMNT   AND R TIME HERE                                
         ZAP   SVYHR,TRNHOUR                                                    
ACTU235  ZAP   SVBUCK6,TRNAMNT  KEEP B+R TOTAL                                  
         ZAP   SVBUCK5,TRNHOUR                                                  
         B     ACTU250                                                          
*                                                                               
ACTU235A CLI   RCSUBPRG,2          TIME SHEET DATA                              
         BNE   ACTU240                                                          
         ZAP   SVHOUR,TRNHOUR   SAVE BTIME HERE                                 
         ZAP   SVRATE,TRNRATE                                                   
         ZAP   SVAMNT,TRNAMNT                                                   
         B     ACTU250                                                          
*                                                                               
ACTU240  CLI   RCSUBPRG,5          AGEING                                       
         BNE   ACTU241                                                          
         LA    R5,MNTH1                                                         
         LA    R6,SVMNTH4                                                       
         LA    R0,3                                                             
*                                                                               
ACTU240A CLC   SVDATE(2),0(R5)                                                  
         BNH   ACTU240B                                                         
         LA    R5,2(R5)                                                         
         SH    R6,=H'8'                                                         
         BCT   R0,ACTU240A                                                      
*                                                                               
ACTU240B ZAP   0(8,R6),TRNHOUR                                                  
         ZAP   SVTOTAL,TRNHOUR                                                  
         B     ACTU250                                                          
*                                                                               
ACTU241  ZAP   SVYAMNT,TRNAMNT     BUMP YTD AMOUNT                              
         ZAP   SVYHR,TRNHOUR     BUMP YTD HOURS                                 
*                                                                               
*        NOTE  FIRST OPERAND IS MODIFIED IN REQUEST FIRST                       
*                                                                               
ACTUCUR  CLC   SVDATE(2),CURMON  THE SAME MONTH AS CURMON                       
         BNE   ACTU250                                                          
         ZAP   SVCAMNT,SVYAMNT     BUMP CURRENT MONTH AMOUNT                    
         ZAP   SVCHR,SVYHR         BUMP CURRENT MONTH HOURS                     
*                                                                               
*                                                                               
ACTU250  CLI   ACPCLEN,X'11'          IS THERE A PROJECT ACCOUNT?               
         BNH   ACTUX                  NO                                        
*                                                                               
         CLC   ACPCPRJT+9(6),SPACES   IS THERE A JOB IN THE PROJECT             
         BE    ACTU260             NO                                           
*                                                                               
         MVC   SVCLI(3),ACPCPRJT+3                                              
         MVC   SVPRO(3),ACPCPRJT+6                                              
         MVC   SVJOB(6),ACPCPRJT+9                                              
         MVC   SVSJ,ACPCPRJT+3                                                  
*                                                                               
         CLC   ACPCTSK,SPACES      KEEP CRAP OUT OF SVTSK                       
         BNH   *+10                                                             
         MVC   SVTSK(2),ACPCTSK                                                 
*                                                                               
         USING ACKEYACC,R1                                                      
ACTU260  L     R1,ADTRANS                                                       
         SH    R1,DATADISP                                                      
         MVC   SVSUBR,ACKEYSBR    NEED TO KEEP TRANS UNIQUE AT JOB              
         DROP  R1                                                               
*                                                                               
ACTUX    BAS   RE,PUTSORT                                                       
         MVC   SVJOB,SPACES        CLEAR OUT SJ LEVEL SORT KEYS                 
         MVC   SVCLI,SPACES                                                     
         MVC   SVTSK(2),SPACES                                                  
         MVC   SVPRO,SPACES                                                     
         XC    SVDATE,SVDATE                                                    
         XC    SVSUBR,SVSUBR                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LAST REQUEST - GET RECORDS FROM SORTER                                 
*                       PRODUCE REPORT                                          
*------------------------------------------------------------------*            
REQLST   DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   RUNLST                                                           
*                                                                               
         MVI   SRTREC,C' '                INIT FOR FIRST THRU                   
         MVC   SRTREC+1(SRTLNQ-1),SRTREC                                        
*                                                                               
REQL10   BAS   RE,GETSORT          RETURN A SORT RECORD IN THISREC              
*                                                                               
         CLC   THISREC(L'SPACES),SPACES ANYTHING RETURNED                       
         BE    XIT                 NO                                           
*                                                                               
         USING SRTD,R6                                                          
         USING TABLED,R2                                                        
         LA    R6,THISREC          A(RECORD TO PROCESS                          
*                                                                               
         USING LISTD,R5                                                         
         LA    R5,LISTDATA                                                      
         LA    R7,P+7                                                           
         SR    R1,R1                                                            
*                                                                               
         MVC   LEVEL,DETAIL                                                     
         BAS   RE,GETLEV                                                        
*                                                                               
         CLI   REPSTAT,TIMESHET                                                 
         BNE   REQL13                                                           
         ZIC   R4,DETOFF           DETAIL DATA IS A DATE                        
         AR    R4,R6                                                            
         MVC   SVDATE,0(R4)                                                     
         B     REQL15                                                           
*                                                                               
REQL13   ZIC   R4,DETOFF           OFFSET INTO KEY OF DETAIL DATA               
         AR    R4,R6                                                            
         MVC   0(4,R7),=C'NONE'                                                 
         CLC   0(12,R4),SPACES     ANYTHING THERE                               
         BE    REQL15                                                           
*                                                                               
         MVC   0(4,R7),SPACES                                                   
         IC    R1,TABLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)                                                    
         AR    R7,R1                                                            
         LA    R7,3(R7)                                                         
*                                                                               
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTNAME,SPACES                                                  
         ZIC   R5,TABDATA                                                       
         AR    R5,R6                                                            
         IC    R1,TABDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDATA(0),0(R5)                                                
*                                                                               
         OC    TABLST,TABLST       TABLE DEFINED FOR THIS LEVEL                 
         BZ    REQL15              NO                                           
*                                                                               
         MVC   *+8(2),TABLST                                                    
         L     R2,FULL                                                          
         BAS   RE,GETLIST                                                       
         LR    R5,R7                                                            
         BAS   RE,CHOPNAME                                                      
*                                                                               
REQL15   LA    R3,SRTBUCKS                                                      
         MVI   MASK,X'FF'                                                       
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         BAS   RE,BUMPTOTS                                                      
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PRODUCE TOTALS, IF NECCESARY                                           
*------------------------------------------------------------------*            
*                                                                               
         ZIC   RF,NUMLEVS          NUMBER OF LEVELS DEFINED (FOR LOOP)          
         BCTR  RF,0                LESS ONE IS THE NUMBER OF TOTALS             
         LTR   RF,RF                                                            
         BZ    REQL190             NO TOTALS DEFINED                            
         LA    R7,TOTLEVS                                                       
*                                                                               
REQL20   ZIC   R1,1(R7)                                                         
         LTR   R1,R1                                                            
         BZ    REQL190             NO TOTALS DEFINED                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISREC(0),SRTREC                                                
         BE    REQL190                                                          
*                                                                               
         MVC   LEVEL,0(R7)                                                      
         BAS   RE,GETLEV                                                        
         LA    R3,TABBUCKS                                                      
         CLI   LEVEL,SVGLEV        SERVICE GROUP LEVEL                          
         BNE   REQL40                                                           
         BAS   RE,CHKSVGP          SEE IF I WANT THIS ONE                       
         BNE   REQL60                                                           
*                                                                               
REQL40   CLI   OPTTOTS,C'Y'        FORCE ALL TOTALS ?                           
         BE    REQL50              YES                                          
*                                                                               
         BAS   RE,CHKTOTS          DO I NEED TOTALS ?                           
         BNE   REQL60              NO                                           
*                                                                               
REQL50   BAS   RE,PRTTOT                                                        
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL60   BAS   RE,ZAPEM                                                         
*                                                                               
         LA    R7,2(R7)                                                         
*                                                                               
         BCT   RF,REQL20                                                        
*                                                                               
REQL190  CLI   SRTREC,X'FF'                                                     
         BE    REQL200                                                          
*                                                                               
         CLC   LEVEL,DETAIL        DID I DO ANY TOTALS                          
         BE    REQL10              NO                                           
*                                                                               
         SH    R7,=H'2'            BACK UP INTO TOTLEVS                         
         MVC   LEVEL,0(R7)                                                      
         BAS   RE,PRTFRST          PRINT ANY FIRSTS YOU MIGHT NEED              
         B     REQL10              GET NEXT SORT REC                            
         DROP  R5                                                               
*                                                                               
*                                                                               
REQL200  EQU   *                   REPORT TOTALS                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   LEVEL,REQLEV                                                     
         LA    R3,REQTOTS                                                       
         MVC   P+1(18),=C'TOTALS FOR REQUEST'                                   
         BAS   RE,PRTBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         BAS   RE,ZAPEM                                                         
*                                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RELEASE BUFFERS                                                        
*-------------------------------------------------------------------*           
RUNLST   CLI   MODE,RUNLAST                                                     
         BNE   *+8                                                              
         BAS   RE,RELBUFF                                                       
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------*         
*        END OF MODE DEPENDENT SUB ROUTINES                                     
*---------------------------------------------------------------------*         
         EJECT                                                                  
*-------------------------------------------------------------------*           
PUTSORT  NTR1                             -- PUT RECORD TO SORT --              
*-------------------------------------------------------------------*           
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         LA    R4,SRTKEY                                                        
         XC    SRTKEY(SRTKEYLN),SRTKEY                                          
         LA    R3,ORDER                                                         
         ZIC   R0,NUMLEVS                                                       
*                                                                               
PUTS50   CLI   0(R3),0             END OF ORDER LIST                            
         BE    PUTSX                                                            
         MVC   LEVEL,0(R3)                                                      
         BAS   RE,GETLEV                                                        
         USING TABLED,R2                                                        
         MVC   PUTS55+4(2),TABMVC                                               
PUTS55   MVC   0(LEVLEN,R4),BYTE                                                
         LA    R3,1(R3)                                                         
         LA    R4,LEVLEN(R4)                                                    
         BCT   R0,PUTS50                                                        
*                                                                               
PUTSX    MVC   SRTSJ,SVSJ          NEED THESE TO RECONSTRUCT ACCOUNTS           
         MVC   SRT1R,SV1R                                                       
         MVC   SRTOFF,SVOFFS                                                    
         MVC   SRTOFG,SVOFG                                                     
         MVC   SRTTSK,SVTSK                                                     
*                                                                               
         LA    R3,SRTBUCKS                                                      
         LA    R0,SBUKCONT                                                      
         LA    R2,SVBUCKS                                                       
*                                                                               
PUTSX60  ZAP   0(BUCKLN,R3),0(BUCKLN,R2)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R0,PUTSX60                                                       
*                                                                               
PUTSXX   GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
         B     XIT                                                              
*                                                                               
PRTR6    NTR1                                                                   
         MVC   P+5(4),ORDER                                                     
         MVC   P+10(1),NUMLEVS                                                  
         MVC   P+12(100),0(R6)                                                  
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
GETSORT  NTR1                      RETURN A SORT RECORD IN THISREC              
*                                  SUM SORT RECORDS WITH DUP KEYS               
*                                  WHEN ALSORT=0 THER ARE NO MORE               
*------------------------------------------------------------*                  
*                                                                               
         MVC   THISREC(SRTLNQ),SRTREC                                           
GETS10   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         LTR   R6,R6                      END OF RECORDS FROM SORT              
         BNZ   GETS20                     NO                                    
         CLC   SRTREC(L'SPACES),SPACES    IS THERE A PREVIOUS                   
         BE    XIT                        NO                                    
         MVI   SRTREC,X'FF'               YES, FORCE LAST'S                     
         B     GETSX                                                            
*                                                                               
GETS20   MVC   SRTREC(SRTLNQ),0(R6)       SAVE CURRENT SORT RECORD              
         CLC   THISREC(L'SPACES),SPACES     DO I HAVE ONE SAVED                 
         BNE   GETS50                     YES - CONTINUE                        
         MVC   LEVEL,ORDER                                                      
         BAS   RE,PRTFRST                                                       
         MVC   THISREC(SRTLNQ),SRTREC     SAVE THIS ONE                         
         B     GETS10                     AND GET NEXT                          
*                                                                               
GETS50   CLC   THISREC(SRTKEYLN),SRTREC  SAME KEY                               
         BNE   GETSX                      NO - PROCESS SAVED ONE                
*                                                                               
         CLI   REPSTAT,TIMESHET    TIME SHEET DATA ?                            
         BE    GETSX               YES, PROCESS RECORDS SEPARTLY                
*                                                                               
         LA    R5,THISREC                 YES - ADD'EM UP                       
         LA    R6,SRTREC                 YES - ADD'EM UP                        
         AP    SRTBUCK1-SRTD(L'SRTBUCK1,R5),SRTBUCK1                            
         AP    SRTBUCK2-SRTD(L'SRTBUCK2,R5),SRTBUCK2                            
         AP    SRTBUCK3-SRTD(L'SRTBUCK3,R5),SRTBUCK3                            
         AP    SRTBUCK4-SRTD(L'SRTBUCK4,R5),SRTBUCK4                            
         AP    SRTBUCK5-SRTD(L'SRTBUCK5,R5),SRTBUCK5                            
         AP    SRTBUCK6-SRTD(L'SRTBUCK6,R5),SRTBUCK6                            
         B     GETS10                     AND GET NEXT                          
*                                                                               
GETSX    LA    R6,THISREC                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
PRTBUCKS NTR1                   -PRINT THE BUCKETS AT 0(R3)                     
*                               -MASK CONTAINS T IF TOTALS                      
*                               -REPSTAT MUST BE SET TO CURRHRS OR              
*                                TIMESHET FOR THE TYPE OF REPORT YOU            
*                                PRODUCING                                      
*                                -PSTART IS A(FIRST COL TO PRINT)               
*------------------------------------------------------------*                  
         L     R4,PSTART                                                        
*                                                                               
         CLI   REPSTAT,CURRHRS     BUT IS THIS THE CURRHRS REPORT               
         BE    PRTB50              YES, PRINT BUCKETS AT FIRST OFFSET           
*                                                                               
         CLI   MASK,C'T'           DOING TOTALS                                 
         BE    PRTB02              YES, NO DATE                                 
*                                                                               
         BAS   RE,PRTDATE          JOB REP, PRINT DATE FIRST                    
*                                                                               
PRTB02   LA    R4,14(R4)          JOB REPORT BUCKETS-2ND OFFSET                 
*                                                                               
         BAS   RE,PRTAMNT                                                       
*                                                                               
PRTB10   LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R4,14(R4)           NEXT PRINT COL                               
         CLI   MASK,C'T'                                                        
         BE    PRTB30                                                           
*                                                                               
         BAS   RE,PRTAMNT                                                       
*                                                                               
PRTB30   LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R4,14(R4)           NEXT PRINT COL                               
         BAS   RE,PRTAMNT          THIRD BUCK ON JOB IS AMOUNT                  
         B     XIT                                                              
*                                                                               
PRTB50   ZIC   R2,NUMCOLS                                                       
*                                                                               
PRTB60   BAS   RE,PRTAMNT                                                       
         LA    R3,BUCKLN(R3)       NEXT BUCK                                    
         LA    R4,14(R4)           NEXT PRINT COL                               
         BCT   R2,PRTB60                                                        
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PRINT ANY FIRSTS YOU MIGHT NEED                                        
*        0(R7) POINTS TO THE SORT KEY AREA                                      
*        PRINT THE NEW FIRST FOR THIS AND LOWER LEVELS                          
*------------------------------------------------------------------*            
PRTFRST  NTR1                                                                   
         LA    R3,ORDER                                                         
         ZIC   R0,TOTNUM                                                        
         LTR   R0,R0                                                            
         BZ    PRFRX               NO TOTALS                                    
*                                                                               
         CLC   LEVEL,0(R3)         POINT R3 INTO ORDER                          
         BE    PRFR10                                                           
         LA    R3,1(R3)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
PRFR10   LA    R7,TOTLEVS                                                       
         ZIC   R1,TOTNUM                                                        
         CLC   LEVEL,0(R7)         POINT R7 INTO TOTLEVS                        
         BE    PRFR30                                                           
         LA    R7,2(R7)                                                         
         BCT   R1,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         USING SRTD,R6                                                          
         USING TABLED,R2                                                        
PRFR30   CLI   0(R3),0                                                          
         BE    PRFRX                                                            
         CLI   0(R3),TMSLEV        TIME SHEET DATA  LEVEL                       
         BE    PRFRX               YES, NO FIRSTS                               
         MVC   LEVEL,0(R3)                                                      
         BAS   RE,GETLEV                                                        
         ST    R2,TABPOINT                                                      
*                                                                               
         MVC   *+8(2),TABMVC       CLEAR SV AREA FOR THIS LEVEL                 
         MVC   BYTE(12),=CL12'NONE'                                             
         MVC   *+8(2),TABNAME                                                   
         MVC   BYTE(36),SPACES                                                  
*                                                                               
         LA    R6,SRTREC           ADDRESS SORT FIELD                           
         IC    R1,1(R7)                                                         
         AR    R6,R1                                                            
         SH    R6,=Y(LEVLEN)                                                    
         CLC   0(LEVLEN,R6),SPACES ANYTHING THERE                               
         BE    PRFR60                                                           
*                                                                               
         USING LISTD,R4                                                         
         MVC   *+8(2),TABMVC     SAVE THIS LEVEL'S DATA                         
         MVC   BYTE(LEVLEN),0(R6)                                               
*                                                                               
         LA    R4,LISTDATA                                                      
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTNAME,SPACES                                                  
*                                                                               
         CLI   LEVEL,SVGLEV        IS THIS A SERVICE GROUP                      
         BNE   PRFR40              NO                                           
         BAS   RE,CHKSVGP          ARE THEY BREAKING ON SERVICE GROUP           
         BNE   PRFR80              NO, SKIP FIRSTS                              
         B     PRFR60              PRINT, BUT DON'T GET NAME                    
*                                                                               
PRFR40   ZIC   R1,TABDLEN                                                       
         BCTR  R1,0                                                             
         ZIC   R5,TABDATA                                                       
         LA    R6,SRTREC                                                        
         AR    R5,R6                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTKEY(0),0(R5)                                                 
*                                                                               
         MVC   *+8(2),TABLST                                                    
         L     R2,FULL                                                          
         LA    R6,LISTDATA                                                      
         BAS   RE,GETLIST                                                       
         L     R2,TABPOINT                                                      
         MVC   *+8(2),TABNAME                                                   
         MVC   BYTE(36),LISTNAME                                                
*                                                                               
PRFR60   CLI   TABPRNT,C'H'        IS THIS HEADER DATA                          
         BE    PRFR70              YES                                          
         CLI   TABPRNT,C'D'        IS THIS DETAIL                               
         BE    PRFRX               YES                                          
*                                                                               
         LA    R4,LISTDATA                                                      
         LA    R5,P+2                                                           
         MVC   0(L'TABTITLE,R5),TABTITLE                                        
         IC    R1,BODYTLN                                                       
         AR    R5,R1                                                            
         LA    R5,1(R5)                                                         
         MVC   *+10(2),TABMVC                                                   
         MVC   0(12,R5),BYTE                                                    
*                                                                               
         CLC   0(4,R5),=C'NONE'   DID I JUST PRINT NONE?                        
         BE    PRFR65              YES,DONT PRINT NAME                          
*                                                                               
         IC    R1,BODYALN                                                       
         AR    R5,R1                                                            
         LA    R5,2(R5)                                                         
         BAS   RE,CHOPNAME                                                      
PRFR65   BAS   RE,PRINTEM                                                       
         B     *+8                                                              
*                                                                               
PRFR70   MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRFR80   LA    R3,1(R3)                                                         
         SH    R7,=H'2'                                                         
         BCT   R0,PRFR30                                                        
*                                                                               
PRFRX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*  PRINT A TOTALS LINE FOR THE TABLE ENTRY  AT 0(R2)                            
*-------------------------------------------------------------------            
         USING TABLED,R2                                                        
PRTTOT   NTR1                                                                   
         L     R4,PSTART                                                        
         SH    R4,=H'2'                                                         
         MVI   0(R4),C'*'          FLAG TOTALS                                  
*                                                                               
         MVC   P+2(9),=C'TOTAL FOR'                                             
         MVC   P+12(L'TABTITLE),TABTITLE                                        
         LA    R5,P+12                                                          
         ZIC   R1,TOTTLN                                                        
         AR    R5,R1                                                            
         MVC   *+10(2),TABMVC                                                   
         MVC   0(12,R5),BYTE                                                    
*                                                                               
         CLI   REPSTAT,TIMESHET    TIME SHEET DATA REPORT                       
         BNE   *+8                 NO                                           
         MVI   MASK,C'T'           YES, ONLY PRINT HOURS AND AMOUNT             
         BAS   RE,PRTBUCKS                                                      
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        CHECK IF I HAVE TO PRINT THIS TOTAL, OR IF I SHOULD SUPRESS            
*        IT                                                                     
*        ON ENTRY                  R7=A(TOTALS I WILL NEED THIS RUN)            
*                                  R2=A(TABLE ENTRY FOR THIS LEVEL)             
*                                  R3=A(BUCKETS I WILL PRINT)                   
*---------------------------------------------------------------------          
         USING TABLED,R2                                                        
CHKTOTS  NTR1                                                                   
         MVC   BYTE,TABTYPE        SAVE TYPE OF CURRENT LEVEL                   
         LA    R7,2(R7)            GET NEXT LEVEL                               
         ZIC   R1,1(R7)                                                         
         LTR   R1,R1                                                            
         BZ    CHKTEQ              NO MORE TOTALS, DO THIS ONE                  
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISREC(0),SRTREC                                                
         BE    CHKTEQ              HIGHER LEV IS THE SAME, DO TOTALS            
*                                                                               
         MVC   LEVEL,0(R7)                                                      
         BAS   RE,GETLEV                                                        
*                                                                               
         CLC   TABTYPE,BYTE        NEXT LEVEL THE SAME ?                        
         BNE   CHKTEQ              NO, FORCE TOTALS                             
*                                                                               
         LA    R4,TABBUCKS         NEXT LEVS BUCKETS                            
         LA    R1,NUMBUCKS                                                      
*                                                                               
CHKT50   CP    0(BUCKLN,R3),0(BUCKLN,R4)                                        
         BNE   CHKTEQ              DIF AMOUNTS, FORCE TOTALS                    
         LA    R3,BUCKLN(R3)                                                    
         LA    R4,BUCKLN(R4)                                                    
         BCT   R1,CHKT50                                                        
*                                                                               
         LTR   RB,RB               SET NEQ CC TO SKIP TOTS                      
         B     *+6                                                              
CHKTEQ   CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------                                       
*  GET ACCOUNT LEVEL DATA, PUT EMPLOYEE--                                       
*  KEY IN SRT RECORD, SAVE EMPLOYEE NAM--                                       
*  IN TABLE.                           --                                       
*--------------------------------------*                                        
NEWACC   NTR1                                                                   
         SR    RF,RF                                                            
         LA    R6,SRTREC                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLEARSV          CLEAR SV AREA TO BUILD SORT                  
*                                                                               
*                                  *SAVE DATA TOU MIGHT NEED FOR SORT           
         USING ACKEYD,R4                                                        
         L     R4,ADACC                                                         
         MVC   SV1R,ACKEYACC+3                                                  
         LA    R1,ACKEYACC+3                                                    
*                                                                               
         USING TABLED,R2                                                        
         MVI   LEVEL,EOFLEV                                                     
         MVC   SV1RLVA,SPACES                                                   
         BAS   RE,GETLEV                                                        
         IC    RF,TABLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SV1RLVA(0),0(R1)                                                 
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVI   LEVEL,DEPLEV                                                     
         MVC   SV1RLVB,SPACES                                                   
         BAS   RE,GETLEV                                                        
         IC    RF,TABLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SV1RLVB(0),0(R1)                                                 
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVI   LEVEL,SDELEV                                                     
         MVC   SV1RLVC,SPACES                                                   
         BAS   RE,GETLEV                                                        
         IC    RF,TABLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SV1RLVC(0),0(R1)                                                 
         LA    R1,1(RF,R1)                                                      
*                                                                               
         MVI   LEVEL,EMPLEV                                                     
         MVC   SV1RLVD,SPACES                                                   
         BAS   RE,GETLEV                                                        
         IC    RF,TABLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SV1RLVD(0),0(R1)                                                 
*                                                                               
         USING LISTD,R2                                                         
         LA    R2,LISTDATA                                                      
         MVC   LISTKEY,ACKEYACC+3                                               
         L     R4,ADACCNAM                                                      
         L     R3,AEMPLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        STORE ACCOUNT NAMES                                                    
*        A(20 EL) IS IN R4, SAVE IN NAMENEXT                                    
*        KEY IS IN 0(R2)                                                        
*        A(BINSRCH TABLE FOR THIS KEY) IS IN R3                                 
*        PUT KEY, NAMENEXT TO BINSRCH AS NMPTKEY, NMPTPTR                       
*        BUMP NAMENEXT                                                          
*----------------------------------------------------------------------         
         USING LISTD,R2                                                         
         USING NMPTD,R5                                                         
BUILDLST NTR1                                                                   
         LA    R2,LISTDATA                                                      
         LA    R5,NMPTDATA                                                      
         MVC   NMPTKEY,LISTKEY                                                  
*                                                                               
         ZIC   R1,1(R4)                                                         
         L     RF,NAMENEXT         MAKE SURE I HAVE ENOUGH ROOM                 
         L     RE,NAMEEND                                                       
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                NAME TABLE IS FULL                           
*                                                                               
         BCTR  R1,0                                                             
         L     RE,NAMENEXT                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       SAVE 20 EL                                   
*                                                                               
         ST    RE,NMPTPTR          SAVE A(WHERE I PUT THIS 20 EL)               
         ST    RF,NAMENEXT         SAVE A(WHERE THE NEXT ONE GOES)              
*                                                                               
         GOTO1 BINADD,DMCB,(R5),(R3)                                            
         B     XIT                 BINADD SETS CC TO EQ IF ADDED                
         DROP  R2,R5                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET ACCOUNT NAMES KEY IS FIRST 12 OF LISTDATA                          
*        TABLE IS AT 0(R2)                                                      
*        RETURN THE NAME IN LISTDATA+12(36)                                     
*----------------------------------------------------------------------         
         USING BIND,R2                                                          
         USING LISTD,R4                                                         
GETLIST  NTR1                                                                   
         LA    R4,LISTDATA                                                      
         MVC   LISTNAME,SPACES                                                  
         CLC   =C'NONE',LISTKEY                                                 
         BE    GETLSTX                                                          
*                                                                               
         LA    R6,BINTABLE                                                      
         MVC   DMCB+8(16),BININ                                                 
         GOTO1 BINSRCH,DMCB,(X'00',(R4)),(R6)                                   
         CLI   DMCB,0                                                           
         BE    GETLST10            FOUND                                        
*                                                                               
         USING TABLED,R2                                                        
         USING LISTD,R4                                                         
GETLST20 BAS   RE,GETLEV                                                        
         MVC   MYKEY,SPACES        NOT FOUND, GET FROM DATAMGR                  
         MVC   MYCUL,SVCUL         ASSUME AN SJ ACCOUNT                         
         MVC   MYACCT(12),LISTKEY                                               
         BAS   RE,GETACCT          SET R4 TO A(20 EL)                           
         MVC   *+8(2),TABLST                                                    
         L     R3,FULL                                                          
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         LA    R2,NMPTDATA                                                      
         LA    R4,LISTDATA                                                      
         B     *+8                                                              
*                                                                               
         USING NMPTD,R2                                                         
GETLST10 L     R2,DMCB                                                          
         LA    R5,LISTNAME                                                      
         ICM   R4,15,NMPTPTR                                                    
         BAS   RE,GETNAME                                                       
         LA    R4,LISTDATA                                                      
         CLI   LEVEL,JOFLEV        SJ OFFICE REQUESTED?                         
         BNE   XIT                 NO                                           
*                                                                               
         USING LISTD,R4                                                         
         LA    R4,LISTDATA         FIRST CHAR OF SJ OFFICE NAME IS OFG          
         MVC   LISTNAME(L'LISTNAME-1),LISTNAME+1                                
         MVI   LISTNAME+L'LISTNAME,C' '                                         
         B     XIT                                                              
*                                                                               
*                                                                               
GETLSTX  B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD AN ITEM TO THE BINSRCH TABLE                                       
*        P1 IS ITEM, P2 IS A(TABLE)                                             
*----------------------------------------------------------------------         
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER, LEN, KEY, MAX                        
         LA    R6,BINTABLE                                                      
         L     R4,0(R1)                                                         
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   BINAOK                                                           
*                                                                               
         CR    RB,R1               TABLE IS FULL, SET NEQ CC                    
         B     BINAX                                                            
*                                                                               
BINAOK   MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CR    RB,RB               SET EQ CC                                    
BINAX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        R4 IS ADDRESS OF THE 20 ELEMENT                                        
*        R5 IS ADDRESS OF 36 BYTE AREA                                          
*----------------------------------------------------------------------         
         USING ACNAMED,R4                                                       
GETNAME  MVC   0(36,R5),SPACES                                                  
         CLI   0(R4),X'20'         MAKE SURE 20 EL WAS PASSED                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,GETNMVC                                                       
         BR    RE                                                               
GETNMVC  MVC   0(0,R5),ACNMNAME                                                 
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CHOPNAME -  0(R5) IS OFFSET INTO 'P'                                   
*                                                                               
*----------------------------------------------------------------------         
CHOPNAME NTR1                                                                   
         USING LISTD,R4                                                         
         LA    R4,LISTDATA                                                      
         ZIC   R2,NAMELEN                                                       
         CLC   LEVEL,DETAIL        AM I PRINTING A DETAIL NAME                  
         BNE   *+8                 NO                                           
         IC    R2,DETLEN           YES, USE DETAIL LENGTH                       
         LA    R1,36                                                            
         CR    R2,R1                                                            
         BNH   CHOPN50                                                          
         LA    R2,36                                                            
*                                                                               
CHOPN50  LA    R3,4                                                             
         GOTO1 CHOPPER,DMCB,(L'LISTNAME,LISTNAME),((R2),0(R5)),(C'P',(RX        
               3))                                                              
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DO OFFICE SECURITY                                                     
*              RETURNS WITH CC EQUAL IF OFFICE IS GOOD                          
*              CC NOT EQUAL IF OFFICE IS NOT WANTED                             
*----------------------------------------------------------------------         
OFFCHK   NTR1                                                                   
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
*                                                                               
         CLI   NEWOFF,C'Y'                 ON NEW OFFICES?                      
         BE    OFFC40                      YES                                  
*                                                                               
         LA    R0,32                                                            
         L     RF,ADOFFLST                                                      
         CLI   0(RF),0                     ANYTHING THERE                       
         BE    OFFCOK                      NO, NO LIMIT ACCESS                  
*                                                                               
OFFC10   CLI   0(RF),0                     END OF LIST                          
         BE    OFFCNG                      OFFICE NOT FOUND                     
*                                                                               
         CLI   0(RF),C'0'                  OFFICE 0                             
         BE    OFFC20                      SKIP                                 
*                                                                               
         CLC   TRNSOFFC(1),0(RF)                                                
         BE    OFFCOK                      FOUND OFFICE IN LIST                 
*                                                                               
OFFC20   LA    RF,1(RF)                                                         
         BCT   R0,OFFC10                                                        
         B     OFFCNG                                                           
*                                                                               
*        NEW OFFICE VALIDATION                                                  
*                                                                               
OFFC40   L     R1,ADOFFALD                 OFFAL BLOCK                          
         USING OFFALD,R1                                                        
         L     RF,OFFAREQL                 LIST OF VALID OFFICES                
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         BZ    OFFCOK                      OK IF NOTHING IN LIST                
*                                                                               
         LA    RF,2(RF)                                                         
OFFC50   CLC   TRNSOFFC(2),0(RF)                                                
         BE    OFFCOK                      FOUND OFFICE IN LIST                 
         LA    RF,2(RF)                                                         
         BCT   R0,OFFC50                                                        
*                                                                               
OFFCNG   CR    RB,R0                       RETURN A NOT EQUAL                   
         B     OFFCX                                                            
*                                                                               
OFFCOK   CR    RB,RB                       RETURN AN EQUAL                      
OFFCX    B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         OC    ABUFF,ABUFF         DO I HAVE A BUFFER                           
         BNZ   GETB05              YES                                          
*                                                                               
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
GETB05   L     R0,ABUFF            CLEAR BUFFER AREA                            
         L     R1,=A(BUFSIZE)                                                   
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         MVCL  R0,R2                                                            
*                                                                               
         L     R5,ABUFF           ADDRESS BUFFER                                
         LA    R0,MAINNUM                                                       
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         USING BIND,R5                                                          
         L     R2,AMAINTAB                                                      
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         XC    BININ,BININ                                                      
         MVC   BINLEN,=A(NMPTDLN)                                               
         MVC   BINDISPK,=A(L'NMPTKEY)                                           
         MVC   BINMAX,MAINMAX                                                   
         A     R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
*                                                                               
         ST    R5,NAMESTRT         GIVE REMAINING SPACE TO NAME TABLE           
         MVC   NAMENEXT,NAMESTRT                                                
         A     R5,=A(NAMESIZE)     CALC ENDING ADDRESS                          
         ST    R5,NAMEEND                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUILD OFFICE NAME TABLE, OFFICE GROUP TABLE                            
*              FIRST CHAR OF OFFICE NAME TABLE IS OFFICE GROUP                  
*----------------------------------------------------------------------         
GETOFF   NTR1                                                                   
         USING ACKEYD,R7                                                        
         LA    R7,MYKEY                                                         
         USING LISTD,R3                                                         
         LA    R3,LISTDATA                                                      
         MVC   LISTKEY,SPACES                                                   
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCUL(1),RCCOMPFL                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVI   OFFSTAT,C' '        FLAG FOR OFFICE GROUPS                       
         BAS   RE,HIGH                                                          
*                                                                               
GETO10   L     R2,ACREC                                                         
         CLC   0(ACOGCODE-ACOGKEY,R2),ACOGKEY                                   
         BNE   GETO20                                                           
         LR    R4,R2                                                            
         LA    R3,LISTDATA                                                      
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(1),ACOGCODE-ACOGKEY(R2)                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         L     R3,AOFGLST                                                       
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVI   OFFSTAT,C'G'                                                     
         BAS   RE,SEQ                                                           
         B     GETO10                                                           
*                                                                               
GETO20   LA    R2,MYKEY                                                         
         MVI   ACOGSREC,ACOGOFF    GO FOR OFFICES NOW                           
         MVC   ACOGOFC,SPACES                                                   
         BAS   RE,HIGH                                                          
GETO30   L     R2,ACREC                                                         
         CLC   0(ACOGCODE-ACOGKEY,R2),ACOGKEY                                   
         BNE   GETO60                                                           
*                                                                               
         LR    R4,R2                                                            
         LA    R3,LISTDATA                                                      
         MVC   LISTKEY,SPACES                                                   
         MVC   LISTKEY(L'ACOGOFC),ACOGOFC-ACOGKEY(R2)                           
*                                                                               
         MVC   WORK,SPACES         CLEAR WORK TO BUILD A 20 EL                  
*                                  WHICH IS OFFICE GROUP, OFF NAME              
         MVI   ELCODE,X'A0'                                                     
         USING ACGPD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   GETO40                                                           
         MVC   WORK+2(L'ACGPCODE),ACGPCODE                                      
*                                                                               
GETO40   LR    R4,R2                                                            
         LA    R5,WORK+2+L'ACGPCODE                                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BAS   RE,GETNAME                                                       
*                                                                               
         ZIC   R1,1(R4)                                                         
         LA    R1,L'ACGPCODE(R1)   CALC LENGTH OF NEW 20 EL                     
         MVI   WORK,X'20'                                                       
         STC   R1,WORK+1                                                        
         LA    R4,WORK                                                          
         L     R3,AOFFLSTS                                                      
         BAS   RE,BUILDLST                                                      
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         BAS   RE,SEQ                                                           
         B     GETO30                                                           
*                                                                               
GETO60   B     XIT                                                              
         EJECT                                                                  
*------------------                                                  *          
PRINTEM  NTR1                                                                   
*------------------                                                             
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
CLEARSV  NTR1                                                                   
*------------------------------------------------------------*                  
         MVI   SVAREA,C' '                                                      
         MVC   SVAREA+1(SVLEN-1),SVAREA                                         
*                                                                               
         LA    R1,SVBUCKS                                                       
         LA    R0,SVNUMBUK                                                      
CLER03   ZAP   0(BUCKLN,R1),=P'0'            CLEAR TO PACKED ZEROS              
         LA    R1,BUCKLN(R1)                 BUMP TO NEXT BUCKET                
         BCT   R0,CLER03                                                        
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        SEE IF THIS SERVICE GROUP NEEDS TOTALS/FIRSTS                          
*        RETURN CC OF EQ IF IN LIST, NEQ IF NOT                                 
*------------------------------------------------------------*                  
CHKSVGP  NTR1                                                                   
         CLC   =C'NONE',SVOFG                                                   
         BE    CHKSVG90                                                         
         LA    R2,SVGLIST                                                       
         LA    R0,L'SVGLIST                                                     
CHKSVG20 CLC   0(1,R2),SVOFG                                                    
         BE    CHKSVGX                                                          
         LA    R2,1(R2)                                                         
         BCT   R0,CHKSVG20                                                      
CHKSVG90 CR    R0,RB                                                            
CHKSVGX  B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        BUMPTOTS - ADD THE SRTREC'S BUCKETS TO ALL THE TOTALS                  
*                   DEFINED FOR THIS REPORT                                     
*------------------------------------------------------------*                  
         USING TABLED,R2                                                        
         USING SRTD,R3                                                          
BUMPTOTS NTR1                                                                   
         LA    R3,THISREC                                                       
         LA    R3,SRTBUCKS                                                      
         LA    R4,ORDER                                                         
BUMP10   CLI   0(R4),0                                                          
         BE    BUMPX                                                            
         MVC   LEVEL,0(R4)                                                      
         BAS   RE,GETLEV                                                        
         CLI   TABPRNT,C'D'        DETAIL                                       
         BE    BUMPX               YES                                          
*                                                                               
BUMP30   LA    R2,TABBUCKS                                                      
         BAS   RE,ADDEM                                                         
         LA    R4,1(R4)                                                         
         B     BUMP10                                                           
*                                                                               
BUMPX    LA    R2,REQTOTS                                                       
         BAS   RE,ADDEM                                                         
         B     XIT                                                              
*                                                                               
ZAPEM    EQU   *                                                                
         LA    R1,NUMBUCKS                                                      
ZAP10    ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,ZAP10                                                         
         BR    RE                                                               
*                                                                               
ADDEM    NTR1                                                                   
         LA    R1,NUMBUCKS                                                      
ADD10    AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R3,BUCKLN(R3)                                                    
         LA    R2,BUCKLN(R2)                                                    
         BCT   R1,ADD10                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        GETLEV - SET R2 TO THE ADDRESS OF THE TABLE ENTRY                      
*                 PASSED IN LEVEL                                               
*------------------------------------------------------------*                  
         USING TABLED,R2                                                        
GETLEV   NTR1                                                                   
         L     R2,ATABLE                                                        
         LA    R0,TABNUM                                                        
GETL10   CLC   LEVEL,0(R2)                                                      
         BE    GETLX                                                            
         LA    R2,TABTABLN(R2)                                                  
         BCT   R0,GETL10                                                        
         DC    H'0'                                                             
GETLX    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*------------------------------------------------------------*                  
*        CALL DATAMRG , MYKEY HAS BEEN SET                                      
*        RETURN IN R4 THE A(20 ELEMENT)                                         
*------------------------------------------------------------*                  
GETACCT  NTR1                                                                   
         LA    R4,BADACCT                                                       
         CLI   LEVEL,CLILEV                                                     
         BE    GETA20                                                           
         CLI   LEVEL,PROLEV                                                     
         BE    GETA20                                                           
         CLI   LEVEL,JOBLEV                                                     
         BNE   GETAX                                                            
GETA20   BAS   RE,READ                                                          
         CLI   DMCB+8,0              TEST FOR ERRORS                            
         BNE   GETAX                 IF ERRORS, RETURN A(DUMMY 20 EL)           
         MVI   ELCODE,X'20'                                                     
         L     R4,ACREC                                                         
         BAS   RE,GETEL                                                         
         BE    GETAX                                                            
         DC    H'0'                                                             
GETAX    XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*------------------------------------------------------------*                  
*        SETORD, SET SORT ORDER                                                 
*------------------------------------------------------------*                  
SETORD   NTR1                                                                   
*--------------------------------*                                              
*  BUILD COMPOSITE OPTIONS RECORD*                                              
*--------------------------------*                                              
*                                                                               
         USING REQUESTD,R3                                                      
         LA    R3,QOPT1                                                         
*                                                                               
         LA    R4,OPTSJ                                                         
         LA    R5,PROSJ                                                         
         LA    R6,REQSJ                                                         
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPT1R                                                         
         LA    R5,PRO1R                                                         
         LA    R6,REQ1R                                                         
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTTSK                                                        
         LA    R5,PROTSK                                                        
         LA    R6,REQTSK                                                        
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTSRT                                                        
         LA    R5,PROSRT                                                        
         LA    R6,REQSRT                                                        
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTTIME                                                       
         LA    R5,PROTIME                                                       
         LA    R6,REQTIME                                                       
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTSVGP                                                       
         LA    R5,PROSVGP                                                       
         LA    R6,SPACES           NO REQUEST SERVICE GROUP OPTION              
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTOFF                                                        
         LA    R5,PROOFF                                                        
         LA    R6,SPACES           NO REQUEST PRINT OFFICES OPTION              
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTTOTS                                                       
         LA    R5,PROTOTS                                                       
         LA    R6,SPACES           NO REQUEST PRINT OFFICES OPTION              
         BAS   RE,SETOPT                                                        
*                                                                               
         LA    R4,OPTBUCK                                                       
         LA    R5,PROBUCK                                                       
         LA    R6,REQBUCK                                                       
         BAS   RE,SETOPT                                                        
*                                                                               
*--------------------------------*                                              
*  BUILD KEY FOR SORT KEY        *                                              
*--------------------------------*                                              
         LA    R2,ORDER                                                         
         XC    ORDER,ORDER                                                      
         XC    HEADORD,HEADORD                                                  
         XC    NUMLEVS,NUMLEVS                                                  
*                                                                               
         CLI   OPTSRT,C'S'         SJ/1R SORT ORDER                             
         BNE   SETO40                                                           
         BAS   RE,SETSJ                                                         
         BAS   RE,SET1R                                                         
         B     SETO50                                                           
*                                                                               
SETO40   BAS   RE,SET1R                                                         
         BAS   RE,SETSJ                                                         
*                                                                               
SETO50   OC    ORDER,ORDER         ANYTHING DEFINED                             
         BZ    XIT                 NO                                           
         BAS   RE,SETTMS           SET TIMESHEET DATA, IF REQUESTED             
         BAS   RE,SETDET           SET DETAIL LEVEL                             
         BAS   RE,SETHEAD          SET THE LEVELS TO BE USED IN HEADER          
         BAS   RE,SETBODY          SET PRINT OFFSETS FOR BODY OF REPORT         
*                                                                               
         MVC   TOTTLN,HEADTLN      SET PRINT OFFSETS FOR TOTALS TO BE           
         CLC   BODYTLN,TOTTLN      THE GREATER OF THE TWO                       
         BNH   *+10                                                             
         MVC   TOTTLN,BODYTLN                                                   
*                                                                               
         MVC   TOTALN,HEADALN                                                   
         CLC   BODYALN,TOTALN                                                   
         BNH   *+10                                                             
         MVC   TOTALN,BODYALN                                                   
*                                                                               
         BAS   RE,SETFOOT          PRINT SORT ORDER IN FOOTER                   
*                                                                               
         B     XIT                                                              
*                                                                               
*-----------------------------------*                                           
* IF REQUEST OPTION EXISTS, USE IT, *                                           
*        ELSE USE PROFILE OPTION    *                                           
*-----------------------------------*                                           
SETOPT   EQU   *                                                                
         CLI   0(R5),C' '                                                       
         BNH   *+10                                                             
         MVC   0(1,R4),0(R5)                                                    
         CLI   0(R6),C' '                                                       
         BNH   *+10                                                             
         MVC   0(1,R4),0(R6)                                                    
         BR    RE                                                               
*                                                                               
*---------------------------------------------------*                           
*    SET ORDER OF SJ DATA THE USER WANTS TO SEE     *                           
*---------------------------------------------------*                           
SETSJ    EQU   *                                                                
         CLI   OPTSJ,C'N'                                                       
         BER   RE                  NO SJ                                        
         CLI   OPTOFF,C'N'         SORT SJ BY OFFICE                            
         BE    SETSJ20             NO                                           
*                                                                               
         CLI   OFFSTAT,C'G'        DO THEY HAVE OFFICE GROUPS                   
         BNE   SETSJ10             NO, JUST SET OFFICE                          
*                                                                               
         MVI   0(R2),JOGLEV                                                     
         BAS   R6,BUMPORD                                                       
*                                                                               
         CLI   OPTSVGP,C'Y'        DO THEY HAVE SERVICE GROUPS                  
         BNE   SETSJ10             NO, JUST SET OFFICE                          
         MVI   0(R2),SVGLEV                                                     
         BAS   R6,BUMPORD                                                       
*                                                                               
SETSJ10  MVI   0(R2),JOFLEV                                                     
         BAS   R6,BUMPORD                                                       
*                                                                               
SETSJ20  CLI   OPTSJ,C'C'          CLIENT REQUEST                               
         BNE   SETSJ30                                                          
         MVI   0(R2),CLILEV                                                     
         BAS   R6,BUMPORD                                                       
         BR    RE                                                               
*                                                                               
SETSJ30  CLI   OPTSJ,C'P'          PRODUCT REQUEST                              
         BNE   SETSJ40                                                          
         MVI   0(R2),CLILEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),PROLEV                                                     
         BAS   R6,BUMPORD                                                       
         BR    RE                                                               
*                                                                               
SETSJ40  CLI   OPTSJ,C'J'          JOB REQUEST                                  
         BNE   SETSJ50                                                          
         MVI   0(R2),CLILEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),PROLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),JOBLEV                                                     
         BAS   R6,BUMPORD                                                       
SETSJ50  BR    RE                                                               
*                                                                               
*---------------------------------------------------*                           
*    SET ORDER OF 1R DATA THE USER WANTS TO SEE     *                           
*        IF TASK HAS BEEN SELECTED, PUT IT BEFORE   *                           
*        OR AFTER 1R                                *                           
*---------------------------------------------------*                           
SET1R    EQU   *                                                                
         CLI   OPTTSK,C'T'         TASK ABOVE 1R                                
         BNE   SET1R20             NO                                           
         MVI   0(R2),TSKLEV                                                     
         BAS   R6,BUMPORD                                                       
*                                                                               
SET1R20  CLI   OPT1R,C'N'          NO 1R                                        
         BE    SET1R100            CHECK FOR TASK BELOW EMP                     
         CLI   OPT1R,C'1'                                                       
         BNE   SET1R30                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         B     SET1R100                                                         
*                                                                               
SET1R30  CLI   OPT1R,C'2'                                                       
         BNE   SET1R40                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),DEPLEV                                                     
         BAS   R6,BUMPORD                                                       
         B     SET1R100                                                         
*                                                                               
SET1R40  CLI   OPT1R,C'3'                                                       
         BNE   SET1R50                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),DEPLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),SDELEV                                                     
         BAS   R6,BUMPORD                                                       
         B     SET1R100                                                         
*                                                                               
SET1R50  CLI   OPT1R,C'4'                                                       
         BNE   SET1R60                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),DEPLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),SDELEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),EMPLEV                                                     
         BAS   R6,BUMPORD                                                       
*                                                                               
*                                                                               
SET1R60  CLI   OPT1R,C'A'          1R LEVELS 1 AND 4                            
         BNE   SET1R70                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),EMP12LEV                                                   
         BAS   R6,BUMPORD                                                       
*                                                                               
SET1R70  CLI   OPT1R,C'B'          1R LEVELS 1,2, AND 4                         
         BNE   SET1R80                                                          
         MVI   0(R2),EOFLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),DEPLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),EMP12LEV                                                   
         BAS   R6,BUMPORD                                                       
*                                                                               
SET1R80  CLI   OPT1R,C'C'          1R LEVELS 3 AND 4                            
         BNE   SET1R90                                                          
         MVI   0(R2),SDELEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   0(R2),EMP12LEV                                                   
         BAS   R6,BUMPORD                                                       
*                                                                               
SET1R90  CLI   OPT1R,C'D'          ONE LINE PER EMPLOYEE                        
         BNE   SET1R100                                                         
         MVI   0(R2),EMP12LEV                                                   
         BAS   R6,BUMPORD                                                       
*                                                                               
SET1R100 CLI   OPTTSK,C'E'         TASK BELOW EMP                               
         BNER  RE                  NO                                           
         MVI   0(R2),TSKLEV                                                     
         BAS   R6,BUMPORD                                                       
         BR    RE                                                               
*                                                                               
SETTMS   EQU   *                   SET TO PRINT TIMESHEET DATA                  
         CLI   OPTBUCK,C'T'                                                     
         BNER  RE                                                               
*                                                                               
         MVI   0(R2),TMSLEV                                                     
         BAS   R6,BUMPORD                                                       
         MVI   RCSUBPRG,2                                                       
         MVI   REPSTAT,TIMESHET                                                 
         MVC   CURMONP,SPACES                                                   
         BR    RE                                                               
*                                                                               
BUMPORD  EQU   *                                                                
         LA    R2,1(R2)                                                         
         ZIC   R1,NUMLEVS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NUMLEVS                                                       
         BR    R6                                                               
*                                                                               
*                                  SET LOWEST LEVEL IN ORDER AS DETAIL          
SETDET   NTR1                                                                   
         USING TABLED,R2                                                        
         LA    R3,ORDER                                                         
         ZIC   R1,NUMLEVS          NUMBER OF LEVELS                             
         BCTR  R1,0                MAKE IT AN OFFSET                            
         STC   R1,TOTNUM                                                        
         AR    R3,R1               POINT TO LOWEST LEVEL DEFINED                
         MVC   LEVEL,0(R3)                                                      
         BAS   RE,GETLEV                                                        
*                                                                               
         MVI   TABPRNT,C'D'                                                     
         MVC   DETAIL,0(R2)        SAVE THE LEVEL THAT IS DETAIL                
         MVC   DETAILP,TABTITLE    SAVE THE NAME OF THE DETAIL LEVEL            
*                                                                               
         XC    TOTSTART,TOTSTART   PRIME START OF TABLE                         
*                                  FOR WHEN I BACK OUT                          
         XC    TOTLEVS(24),TOTLEVS SET LEVELS I WILL NEED TOTALS FOR            
         LTR   R4,R1                                                            
         BZ    SETDX               NO TOTALS                                    
*                                                                               
         MH    R4,=Y(LEVLEN)                                                    
         STC   R4,DETOFF                                                        
         LA    R2,TOTLEVS                                                       
SETD40   BCTR  R3,0                DEC R3                                       
         MVC   0(1,R2),0(R3)                                                    
         STC   R4,1(R2)                                                         
         LA    R2,2(R2)                                                         
         SH    R4,=Y(LEVLEN)                                                    
         BCT   R1,SETD40                                                        
*                                                                               
SETDX    MVC   DETAILL,=C'LEVEL DETAIL'                                         
         LA    R4,L'DETAILPR                                                    
         GOTO1 SQUASHER,DMCB,DETAILPR,(R4)                                      
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        SET HEADER INFORMATION                                                 
*------------------------------------------------------------*                  
         USING TABLED,R2                                                        
SETHEAD  NTR1                                                                   
         XC    HEADORD,HEADORD                                                  
         XC    HEADTLN,HEADTLN                                                  
         XC    HEADALN,HEADALN                                                  
         MVC   LEVEL,ORDER         GET HIGHEST LEVEL                            
         BAS   RE,GETLEV                                                        
         BAS   RE,HEADMAX                                                       
*                                                                               
SETH30   CLI   TABPRNT,C'D'        IS THIS LEVEL DETAIL                         
         BE    SETHX               YES, NO HEADERS                              
*                                                                               
         MVC   MYBYTE,TABTYPE      SAVE TYPE OF HIGHEST LEVEL                   
         MVI   TABPRNT,C'H'        SET AS HEADER DATA                           
         MVC   HEADORD(1),LEVEL                                                 
         LA    R3,HEADORD+1        NEXT SPOT IN HEADORD                         
         LA    R0,L'HEADORD-1                                                   
         LA    R4,ORDER+1          NEXT LEVEL IN ORDER                          
*                                                                               
SETH35   CLI   0(R4),0             IS NEXT LEVEL DEFINED                        
         BE    SETHX               NO                                           
         MVC   LEVEL,0(R4)                                                      
         BAS   RE,GETLEV                                                        
         BAS   RE,HEADMAX                                                       
*                                                                               
         CLI   TABPRNT,C'D'        IS THIS LEVEL DETAIL                         
         BE    SETHX               YES, NO MORE HEADER INFO                     
         CLI   TABLEV,SDELEV       IS THIS SUBDEP LEV                           
         BE    SETHX               YES, NO MORE HEADER INFO                     
         CLC   MYBYTE,TABTYPE      IS THIS THE SAME TYPE AS PREVIOUS            
         BNE   SETHX               NO                                           
*                                                                               
         MVI   TABPRNT,C'H'        SET AS HEADER DATA                           
         MVC   0(1,R3),0(R4)       SAVE THIS LEVEL IN HEADORD                   
         LA    R3,1(R3)            NEXT SPOT IN HEADORD                         
         LA    R4,1(R4)            NEXT LEVEL IN ORDER                          
         BCT   R0,SETH35           DO UNTIL HEADORD IS FILLED OR                
SETHX    B     XIT                 TYPE OF LEVEL CHANGES                        
         EJECT                                                                  
*------------------------------------------------------------------*            
*        SET PRINT OFFSETS FOR HEADER DATA                                      
*------------------------------------------------------------------*            
HEADMAX  EQU   *                                                                
         LA    R5,TABTITLE                                                      
         LA    R6,TABTITLE+L'TABTITLE                                           
         LA    R1,L'TABTITLE                                                    
HDM10    CLI   0(R6),C' '                                                       
         BH    HDM30                                                            
         BCTR  R6,0                                                             
         BCT   R1,HDM10                                                         
*                                                                               
HDM30    LA    R1,2(R1)                                                         
         CLM   R1,X'01',HEADTLN                                                 
         BNH   HDM40                                                            
         STC   R1,HEADTLN                                                       
*                                                                               
HDM40    CLC   HEADALN,TABLN                                                    
         BNL   *+10                                                             
         MVC   HEADALN,TABLN                                                    
         BR    RE                                                               
*------------------------------------------------------------------*            
         EJECT                                                                  
*------------------------------------------------------------------*            
*        SET PRINT OFFSETS FOR BODY OF REPORT                                   
*------------------------------------------------------------------*            
SETBODY  NTR1                                                                   
         XC    BODYTLN,BODYTLN                                                  
         MVI   BODYALN,4           MIN LEN IS 4 IN CASE ITS "NONE"              
         LA    R3,HEADORD                                                       
         LA    R4,ORDER                                                         
         LA    R0,L'HEADORD                                                     
*                                                                               
SETB10   CLI   0(R3),0             GET INTO THE BODY SECTION OF ORDER           
         BE    SETB20                                                           
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,SETB10                                                        
*                                                                               
SETB20   CLI   0(R4),0             END OF ORDER                                 
         BE    SETBX                                                            
*                                                                               
         MVC   LEVEL,0(R4)                                                      
         BAS   RE,GETLEV                                                        
         CLI   TABPRNT,C'D'        DETAIL LEVEL                                 
         BE    SETBX               YES, I'M DONE                                
*                                                                               
         LA    R5,TABTITLE+L'TABTITLE   GET LENGTH OF TITLE                     
         LA    R1,L'TABTITLE                                                    
SETB40   CLI   0(R5),C' '                                                       
         BH    SETB50                                                           
*                                                                               
         BCTR  R5,0                                                             
         BCT   R1,SETB40                                                        
*                                                                               
SETB50   LA    R1,2(R1)                                                         
         CLM   R1,X'01',BODYTLN                                                 
         BNH   SETB60                                                           
         STC   R1,BODYTLN                                                       
*                                                                               
SETB60   CLC   BODYALN,TABLN                                                    
         BNL   *+10                                                             
         MVC   BODYALN,TABLN                                                    
*                                                                               
         LA    R4,1(R4)                                                         
         B     SETB20                                                           
*                                                                               
SETBX    B     XIT                                                              
*------------------------------------------------------------------*            
         EJECT                                                                  
*------------------------------------------------------------*                  
*        PRINT SORT ORDER IN THE FOOTER OF THE REPORT                           
*------------------------------------------------------------*                  
         USING TABLED,R2                                                        
SETFOOT  NTR1                                                                   
         MVC   FOOT1,SPACES                                                     
         MVC   FOOT2,SPACES                                                     
         MVC   FOOT1(11),=C'SORT ORDER:'                                        
         LA    R3,ORDER                                                         
         LA    R0,L'ORDER                                                       
         LA    R4,FOOT1+12                                                      
         LA    R5,FOOT2+L'FOOT2-L'TABTITLE SET END OF FOOT AREA                 
*                                                                               
SETF10   CLI   0(R3),0                                                          
         BE    SETFX                                                            
*                                                                               
         CR    R4,R5               ENOUGH ROOM FOR A TABTITLE HERE              
         BH    SETFXX              NO, LEAVE LAST COMMA                         
*                                                                               
         MVC   LEVEL,0(R3)                                                      
         BAS   RE,GETLEV                                                        
*                                                                               
         MVC   0(L'TABTITLE,R4),TABTITLE                                        
         LA    R4,L'TABTITLE(R4)                                                
         LA    R1,L'TABTITLE                                                    
SETF20   CLI   0(R4),C' '                                                       
         BNE   SETF30                                                           
         BCTR  R4,0                                                             
         BCT   R1,SETF20                                                        
*                                                                               
SETF30   MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,SETF10                                                        
*                                                                               
SETFX    BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
SETFXX   MVI   CLEARFUT,C'N'                                                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*        INITIALIZE ALL VALUES IN TABLE                                         
*------------------------------------------------------------*                  
         USING TABLED,R2                                                        
INITTAB  NTR1                                                                   
         L     R2,ATABLE                                                        
         LA    R0,TABNUM                                                        
INITT10  LA    R3,TABBUCKS                                                      
         BAS   RE,ZAPEM                                                         
         MVI   TABPRNT,C'N'                                                     
         LA    R2,TABTABLN(R2)                                                  
         BCT   R0,INITT10                                                       
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------*                  
*              DATAMGR INTERFACE                                                
*------------------------------------------------------------*                  
HIGH     MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'            READ SEQUENTIAL                    
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'            A SPECIFIC READ                    
*                                                                               
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R7)                      
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
*------------------------------------------------------------*                  
         EJECT                                                                  
*------------------------------------------------------------*                  
*         EDIT ROUTINES - EDIT THE BUCKET AT 0(R3)                              
*                         0(R2) IS THE OFFSET INTO 'P' TO PRINT                 
*------------------------------------------------------------*                  
*                                                                               
*                                                                               
PRTAMNT  EDIT  (P8,(R3)),(13,(R4)),2,MINUS=YES                                  
         BR    RE                                                               
*                                                                               
PRTDATE  ST    RE,SAVERE                                                        
         GOTO1 DATCON,DMCB,(1,SVDATE),(5,(R4))                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*------------------------------------------------------------*                  
*              CONSTANTS                                                        
*------------------------------------------------------------*                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(TABLE)                                                         
         DC    A(MAINTAB)                                                       
         DC    V(SORTER)                                                        
         DC    V(UNDERLIN)                                                      
         DC    V(ACLIST)                                                        
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
BADACCT  DC    X'2023',CL33'-- WARNING - ACCOUNT NOT FOUND --'                  
OUTSIDE  DC    AL1(0)              DUMMY TOTTBL RECORD                          
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
*        TABLE OF BUCKET OFFSETS FOR PRODUCT AND JOB TYPE REPORT                
*                                                                               
*-------------------------------------------------------------------            
         EJECT                                                                  
*--------------------------------------------------------------------*          
         LTORG                                                                  
*--------------------------------------------------------------------*          
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
*--------------------------------------------------------------------*          
*              BOX ROUTINES (HOOK)                                              
*--------------------------------------------------------------------*          
*                                                                               
BXHOOK   DS    0D                                                               
*                                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC           RESTORE REG C                                 
*                                                                               
*                           S E T  H E A D E R  I N F O                         
*                                                                               
         CLI   LEVEL,REQLEV        REQUEST LEVEL HEADER                         
         BE    BX100               YES, NO DETAIL                               
*                                                                               
         USING TABLED,R2                                                        
         LA    R3,HEADORD                                                       
         LA    R1,L'HEADORD                                                     
         LA    R6,HEAD3+2                                                       
BX05     L     R2,ATABLE                                                        
         LA    R0,TABNUM                                                        
         CLI   0(R3),0                                                          
         BE    BX100                                                            
BX10     CLC   TABLEV,0(R3)                                                     
         BE    BX20                                                             
         LA    R2,TABTABLN(R2)                                                  
         BCT   R0,BX10                                                          
         B     BX100                                                            
*                                                                               
BX20     CLI   0(R2),SVGLEV        SERVICE GROUP IN HEADER                      
         BNE   BX25                                                             
         BAS   RE,BXSVGP           SEE IF THEY WANT IT                          
         BNE   BX50                NOT FOUND IN LIST, SO SKIP                   
*                                                                               
BX25     LR    R4,R6                                                            
         MVC   0(L'TABTITLE,R4),TABTITLE                                        
         ZIC   R5,HEADTLN                                                       
         AR    R4,R5                                                            
         LA    R4,1(R4)                                                         
         MVC   BX30+4(L'TABMVC),TABMVC                                          
BX30     MVC   0(12,R4),BYTE                                                    
         ZIC   R5,HEADALN                                                       
         AR    R4,R5                                                            
         LA    R4,2(R4)                                                         
         MVC   BX40+4(L'TABNAME),TABNAME                                        
BX40     MVC   0(30,R4),BYTE                                                    
         LA    R6,L'HEAD4(R6)                                                   
BX50     LA    R3,1(R3)                                                         
         BCT   R1,BX05                                                          
*                                                                               
BX100    CLI   RCSUBPRG,0                                                       
         BNE   *+10                                                             
         MVC   HEAD4+71(23),CURMONP CURRENT MONTH OF ..                         
*                                                                               
         CLI   RCSUBPRG,5          AGEING                                       
         BNE   BOXHX               NO                                           
*                                                                               
         USING AHEADD,R6                                                        
         L     R6,=A(AHEADS)                                                    
         MVC   HEAD8+62(L'AHLINE1),AHLINE1                                      
         MVC   HEAD9+62(L'AHLINE2),AHLINE2                                      
*                                                                               
BOXHX    MVC   HEAD4+114(6),FRMMONP POSTINGS FROM ...                           
         MVC   HEAD4+126(6),THRMONP THRU ...                                    
         MVC   HEAD5+56(20),TDATEP                                              
*                                                                               
         MVC   HEAD8+3(L'DETAILPR),DETAILPR                                     
*                                                                               
         L     R2,=A(TIMEHEAD)                                                  
         LA    R0,THEADNUM                                                      
*                                                                               
BOXHX10  CLC   TIMETYPE,0(R2)      PRINT TYPE OF TIME ON THIS REPORT            
         BE    BOXHX20                                                          
         LA    R2,THEADLN(R2)                                                   
         BCT   R0,BOXHX10                                                       
         B     *+10                                                             
*                                                                               
BOXHX20  MVC   HEAD6+59(13),1(R2)                                               
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'        SET LH MARGIN                                
         ZIC   R2,NUMCOLS          NUMBER OF COLS                               
         LA    R1,BOXCOLS+131      1ST LINE AT 62                               
         MVI   BOXCOLS+131,C'R'                                                 
*                                                                               
BOXIT    SH    R1,=H'14'                                                        
         MVI   0(R1),C'C'                                                       
         BCT   R2,BOXIT                                                         
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BXSVGP   NTR1                                                                   
         CLC   =C'NONE',SVOFG                                                   
         BE    BXSVGP99                                                         
*                                                                               
         LA    R2,SVGLIST                                                       
         LA    R0,L'SVGLIST                                                     
BXSVGP20 CLC   0(1,R2),SVOFG                                                    
         BE    BXSVGPX                                                          
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,BXSVGP20                                                      
*                                                                               
BXSVGP99 CR    R0,RB                                                            
*                                                                               
BXSVGPX  B     XIT                                                              
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*        TYPE OF TIME ON THIS REPORT                                            
*                                                                               
TIMEHEAD DC    AL1(ACPSRTE+ACPSBIL)                                             
         DC    CL13'B+R TIME ONLY'                                              
THEADLN  EQU   *-TIMEHEAD                                                       
         DC    AL1(ACPSNOT)                                                     
         DC    CL13' N TIME ONLY'                                               
         DC    AL1(ACPSBIL)                                                     
         DC    CL13' B TIME ONLY'                                               
         DC    AL1(ACPSRTE)                                                     
         DC    CL13' R TIME ONLY'                                               
         DC    AL1(ACPSRTE+ACPSBIL+ACPSNOT)                                     
         DC    CL13'  ALL TIME  ' ALL TIME                                      
THEADNUM EQU   (*-TIMEHEAD)/THEADLN                                             
         DROP  R3                                                               
         EJECT                                                                  
*------------------------------------------------------------*                  
*        FIND OUT HOW MUCH ROOM YOU HAVE TO PRINT THE NAME                      
*        L'P - L'TITLE - L'DATA - 5 SPACES - (NUMCOLS*13)                       
*                                                                               
*------------------------------------------------------------*                  
SETLEN   NMOD1 0,*SETLN*                                                        
         L     RC,0(R1)            RC IS P1                                     
         SR    R1,R1                                                            
         LA    R1,L'P                                                           
         ZIC   R2,BODYTLN                                                       
         SR    R1,R2                                                            
         IC    R2,BODYALN                                                       
         SR    R1,R2                                                            
         LA    R2,6                VARIOUS SPACES                               
         SR    R1,R2                                                            
         IC    R2,NUMCOLS                                                       
         MH    R2,=H'14'           COLUMN WIDTH                                 
         SR    R1,R2                                                            
         STC   R1,NAMELEN                                                       
         SH    R1,=H'5'                                                         
         STC   R1,DETLEN                                                        
*                                                                               
         CLI   NUMCOLS,4                                                        
         BNE   SETL50                                                           
         LA    R2,P+76                                                          
         ST    R2,PSTART                                                        
*                                                                               
SETL50   CLI   NUMCOLS,5                                                        
         BNE   SETL60                                                           
         LA    R2,P+62                                                          
         ST    R2,PSTART                                                        
*                                                                               
SETL60   CLI   NUMCOLS,6                                                        
         BNE   SETLX                                                            
         LA    R2,P+48                                                          
         ST    R2,PSTART                                                        
*                                                                               
SETLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GO TO GETPROF WITH A COMPOSITE KEY SO IT RETURNS YOU A                 
*        COMPOSITE PROFILE.                                                     
*        P1 IS RC                                                               
*        P2 IS A  OF ACCOUNT                                                    
*        0(R3) IS A(ACCOUNT YOU WANT TO GET A PROFILE FOR)                      
*----------------------------------------------------------------------         
BUILDPRO NMOD1 0,*BPROF                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING PROFKD,R2                                                        
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROFILES,PROFILES                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0R7'                                                 
         MVC   PROFKAGY,ALPHAID                                                 
         MVC   PROFKUNL,=C'1R'                                                  
         LTR   R3,R3               WAS AN ACCOUNT PASSED                        
         BZ    *+10                NO                                           
         MVC   PROFKACC,0(R3)                                                   
         GOTO1 GETPROF,DMCB,PROFKEY,PROFILES,DATAMGR                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        BUILD AGEING MONTH TABLE AND AGEING HEADERS                            
*---------------------------------------------------------------------          
AGEHEADS NMOD1 0,AGHEAD                                                         
         L     RC,0(R1)            RC IS P1                                     
         USING AHEADD,R6                                                        
         L     R6,=A(AHEADS)                                                    
         MVC   AHLINE1,SPACES                                                   
         MVC   AHLINE2,SPACES                                                   
*                                                                               
         MVC   AHSTART2(10),=C'AND BEFORE'                                      
         MVC   AHEND2(9),=C'AND AFTER'                                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK+6)                                
         MVC   WORK+10(2),=C'01'                                                
         CLC   QEND,SPACES                     END DATE SPECIFIED               
         BE    AGEHAA                          NO, USE THIS MONTH               
         MVC   WORK+6(4),QEND                                                   
*                                                                               
AGEHAA   GOTO1 DATCON,DMCB,(0,WORK+6),(6,AHEND)    END DATE                     
         GOTO1 =V(HEXIN),DMCB,WORK+6,MNTH4,4                                    
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'-80' BACK 3 MONTHS FOR START            
         CLC   QSTART,SPACES                                                    
         BE    *+20                                                             
         CLC   QSTART(4),WORK                                                   
         BH    *+10                                                             
         MVC   WORK(4),QSTART                 MUST BE AT LEAST 3 MONTHS         
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,AHSTART)                                 
         GOTO1 =V(HEXIN),DMCB,WORK,MNTH1,4                                      
*                                                                               
         MVC   WORK+18(6),WORK                SAVE START                        
         XR    R3,R3                                                            
*                                                                               
AGEHA1   GOTO1 ADDAY,DMCB,WORK,WORK+12,F'35'                                    
         CLC   WORK+12(4),WORK+6                                                
         BE    AGEHA2                                                           
         LA    R3,1(R3)                       COUNT MONTHS TO END               
         MVC   WORK(4),WORK+12                                                  
         MVC   WORK+4(2),=C'01'                                                 
         B     AGEHA1                                                           
*                                                                               
AGEHA2   MVC   WORK(6),WORK+18                START                             
         XR    R2,R2                                                            
         D     R2,=F'2'                       R3 NUMBER COLUMN 3                
         AR    R2,R3                          R2 NUMBER IN COLUMN 2             
*                                                                               
         LA    R4,AHCOL3                                                        
         LA    R7,2                                                             
         LA    R5,MNTH2                                                         
AGEHA3   GOTO1 ADDAY,DMCB,WORK,WORK+18,F'35' START OF NEXT MONTH                
         MVC   WORK+22(2),=C'01'                                                
         GOTO1 DATCON,DMCB,(0,WORK+18),(6,0(R4))                                
         GOTO1 =V(HEXIN),DMCB,WORK+18,(R5),4                                    
         BCT   R2,*+8                                                           
         B     AGEHA5                                                           
*                                                                               
         MVC   L'AHLINE1(5,R4),=C'  TO '                                        
AGEHA4   GOTO1 ADDAY,DMCB,WORK+18,WORK,F'35'                                    
         MVC   WORK+18(4),WORK                GET END MONTH                     
         MVC   WORK+22(2),=C'01'              FOR COLUMN 2 + 3                  
         BCT   R2,AGEHA4                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+18),(6,L'AHLINE1+5(R4))                      
         GOTO1 =V(HEXIN),DMCB,WORK+18,(R5),4                                    
*                                                                               
AGEHA5   MVC   WORK(6),WORK+18                                                  
         SH    R4,=H'14'                                                        
         LA    R5,2(R5)                                                         
         LR    R2,R3                          COLUMN 3                          
         BCT   R7,AGEHA3                                                        
         MVC   AHTOTAL(11),=C' CUMULATIVE'                                      
         MVC   AHTOTAL2(12),=C'    TOTAL   '                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        TABLE LINKING A SORT LEVEL TO WHERE THE DATA FOR IT IS STORED          
*---------------------------------------------------------------------*         
TABLE    DS    0C                                                               
         DC    AL1(TSKLEV),SL2(SVTSK),AL1(2),SL2(NMTSK)                         
         DC    CL15'TASK',6PL8'0',CL1' ',CL1'T'                                 
         DC    AL1(SRTTSK-SRTD),AL1(2),SL2(ATSKLST)                             
*                                                                               
         DC    AL1(EMPLEV),SL2(SV1RLVD),AL1(7),SL2(NM1RLVD)                     
         DC    CL15' ',6PL8'0',CL1' ',CL1'1'                                    
         DC    AL1(SRT1R-SRTD),AL1(12),SL2(AEMPLST)                             
*                                                                               
         DC    AL1(SDELEV),SL2(SV1RLVC),AL1(2),SL2(NM1RLVC)                     
         DC    CL15' ',6PL8'0',CL1' ',CL1'1'                                    
         DC    AL1(SRT1R-SRTD),AL1(5),SL2(ASDELST)                              
*                                                                               
         DC    AL1(DEPLEV),SL2(SV1RLVB),AL1(2),SL2(NM1RLVB)                     
         DC    CL15' ',6PL8'0',CL1' ',CL1'1'                                    
         DC    AL1(SRT1R-SRTD),AL1(3),SL2(ADEPLST)                              
*                                                                               
         DC    AL1(EOFLEV),SL2(SV1RLVA),AL1(1),SL2(NM1RLVA)                     
         DC    CL15' ',6PL8'0',CL1' ',CL1'1'                                    
         DC    AL1(SRT1R-SRTD),AL1(1),SL2(AOFFLST)                              
*                                                                               
         DC    AL1(JOBLEV),SL2(SVJOB),AL1(6),SL2(NMJOB)                         
         DC    CL15'JOB ',6PL8'0',CL1' ',CL1'S'                                 
         DC    AL1(SRTSJ-SRTD),AL1(12),SL2(AJOBLST)                             
*                                                                               
         DC    AL1(PROLEV),SL2(SVPRO),AL1(3),SL2(NMPRO)                         
         DC    CL15'PRODUCT ',6PL8'0',CL1' ',CL1'S'                             
         DC    AL1(SRTSJ-SRTD),AL1(6),SL2(APROLST)                              
*                                                                               
         DC    AL1(CLILEV),SL2(SVCLI),AL1(3),SL2(NMCLI)                         
         DC    CL15'CLIENT ',6PL8'0',CL1' ',CL1'S'                              
         DC    AL1(SRTSJ-SRTD),AL1(3),SL2(ACLILST)                              
*                                                                               
         DC    AL1(JOFLEV),SL2(SVOFFS),AL1(2),SL2(NMOFFS)                       
         DC    CL15'OFFICE ',6PL8'0',CL1' ',CL1'S'                              
         DC    AL1(SRTOFF-SRTD),AL1(2),SL2(AOFFLSTS)                            
*                                                                               
         DC    AL1(SVGLEV),SL2(SVSVG),AL1(1),SL2(NMSVG)                         
         DC    CL15'SERVICE GROUP',6PL8'0',CL1' ',CL1'S'                        
         DC    AL1(SRTOFF-SRTD),AL1(1),SL2(0)                                   
*                                                                               
         DC    AL1(JOGLEV),SL2(SVOFG),AL1(1),SL2(NMOFG)                         
         DC    CL15'OFFICE GROUP ',6PL8'0',CL1' ',CL1'S'                        
         DC    AL1(SRTOFG-SRTD),AL1(1),SL2(AOFGLST)                             
*                                                                               
         DC    AL1(EMP12LEV),SL2(SV1R),AL1(12),SL2(NM1RLVD)                     
         DC    CL15'EMPLOYEE ',6PL8'0',CL1' ',CL1'1'                            
         DC    AL1(SRT1R-SRTD),AL1(12),SL2(AEMPLST)                             
*                                                                               
         DC    AL1(TMSLEV),SL2(SVTRAN),AL1(4),SL2(0)                            
         DC    CL15'TIME SHEET',6PL8'0',CL1' ',CL1'1'                           
         DC    AL1(0),AL1(0),SL2(0)                                             
*                                                                               
TABNUM   EQU   (*-TABLE)/TABTABLN                                               
NUMBUCKS EQU   6                                                                
         EJECT                                                                  
AHEADS   DS    (AHLEN)C            SPACE TO BUILD AGEING HEADERS                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(AOFGLST)                                                       
         DC    H'0'                                                             
         DC    A(OFGMAX)                                                        
         DC    A(OFGSIZE)                                                       
*                                                                               
         DC    S(AOFFLSTS)                                                      
         DC    H'0'                                                             
         DC    A(OFFMAXS)                                                       
         DC    A(OFFSIZES)                                                      
*                                                                               
         DC    S(ACLILST)                                                       
         DC    H'0'                                                             
         DC    A(CLIMAX)                                                        
         DC    A(CLISIZE)                                                       
*                                                                               
         DC    S(APROLST)                                                       
         DC    H'0'                                                             
         DC    A(PROMAX)                                                        
         DC    A(PROSIZE)                                                       
*                                                                               
         DC    S(AJOBLST)                                                       
         DC    H'0'                                                             
         DC    A(JOBMAX)                                                        
         DC    A(JOBSIZE)                                                       
*                                                                               
         DC    S(AOFFLST)                                                       
         DC    H'0'                                                             
         DC    A(OFFMAX)                                                        
         DC    A(OFFSIZE)                                                       
*                                                                               
         DC    S(ADEPLST)                                                       
         DC    H'0'                                                             
         DC    A(DEPMAX)                                                        
         DC    A(DEPSIZE)                                                       
*                                                                               
         DC    S(ASDELST)                                                       
         DC    H'0'                                                             
         DC    A(SDEMAX)                                                        
         DC    A(SDESIZE)                                                       
*                                                                               
         DC    S(AEMPLST)                                                       
         DC    H'0'                                                             
         DC    A(EMPMAX)                                                        
         DC    A(EMPSIZE)                                                       
*                                                                               
         DC    S(ATSKLST)                                                       
         DC    H'0'                                                             
         DC    A(TSKMAX)                                                        
         DC    A(TSKSIZE)                                                       
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
EMPMAX   EQU   3500                                                             
EMPSIZE  EQU   BINLENQ+(EMPMAX*NMPTDLN)                                         
*                                                                               
TSKMAX   EQU   735                                                              
TSKSIZE  EQU   BINLENQ+(TSKMAX*NMPTDLN)                                         
*                                                                               
SDEMAX   EQU   2000                                                             
SDESIZE  EQU   BINLENQ+(SDEMAX*NMPTDLN)                                         
*                                                                               
DEPMAX   EQU   300                                                              
DEPSIZE  EQU   BINLENQ+(DEPMAX*NMPTDLN)                                         
*                                                                               
OFFMAX   EQU   100                                                              
OFFSIZE  EQU   BINLENQ+(OFFMAX*NMPTDLN)                                         
*                                                                               
OFFMAXS  EQU   255                                                              
OFFSIZES EQU   BINLENQ+(OFFMAXS*NMPTDLN)                                        
*                                                                               
OFGMAX   EQU   50                                                               
OFGSIZE  EQU   BINLENQ+(OFGMAX*NMPTDLN)                                         
*                                                                               
JOBMAX   EQU   4000                                                             
JOBSIZE  EQU   BINLENQ+(JOBMAX*NMPTDLN)                                         
*                                                                               
PROMAX   EQU   2001                                                             
PROSIZE  EQU   BINLENQ+(PROMAX*NMPTDLN)                                         
*                                                                               
CLIMAX   EQU   2001                                                             
CLISIZE  EQU   BINLENQ+(CLIMAX*NMPTDLN)                                         
*                                                                               
NAMESIZE EQU   400000                                                           
*                                                                               
BUFSIZE  EQU   EMPSIZE+SDESIZE+DEPSIZE+OFFSIZE+OFGSIZE+OFFSIZES+JOBSIZEX        
               +PROSIZE+CLISIZE+NAMESIZE                                        
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
*                                                                               
ACR7D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ATABLE   DS    A                                                                
AMAINTAB DS    A                                                                
SORTER   DS    V                                                                
UNDERLIN DS    V                                                                
ACLIST   DS    V                                                                
SQUASHER DS    V                                                                
*                                                                               
AOFGLST  DS    A                   THESE ADDRESSES SET BY GETBUFF               
AOFFLSTS DS    A                                                                
ACLILST  DS    A                                                                
APROLST  DS    A                                                                
AJOBLST  DS    A                                                                
AOFFLST  DS    A                                                                
ADEPLST  DS    A                                                                
ASDELST  DS    A                                                                
AEMPLST  DS    A                                                                
ATSKLST  DS    A                                                                
*                                                                               
ABUFF    DS    A                                                                
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
PSTART   DS    A                   ADDRESS TO PRINT FIRST BUCKET                
NAMESTRT DS    A                   START OF NAME TABLE                          
NAMEEND  DS    A                   END OF NAME TABLE                            
NAMENEXT DS    A                   NEXT AVAILABLE AREA IN TABLE                 
NAMETEMP DS    A                   ADDRESS AFTER OFFICE NAMES                   
ORDERPTR DS    A                   POINTER FOR ORDER TABLE                      
TABPOINT DS    A                   CURRENT TABLE ADDRESS                        
ALSORT   DS    A                   A(LAST SORT RECORD)                          
NUMCOLS  DS    CL1                 NUMBER OF COLS FOR THIS REPORT               
NAMELEN  DS    CL1                 MAX NAME LENGTH FOR CHOPPER/MVC              
DETLEN   DS    CL1                 MAX LENGTH OF A DETAIL NAME                  
MYBYTE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
*                                                                               
ORDER    DS    CL12                SORT ORDER, BY LEVEL EQUATE                  
NUMLEVS  DS    CL1                 NUMBER OF SORT LEVELS IN ORDER               
LEVEL    DS    CL1                 WHAT LEVEL I'M AT                            
DETAIL   DS    CL1                 DETAIL FOR THIS REPORT                       
DETOFF   DS    CL1                 OFFSET TO DETAIL DATA IN SORT REC            
HEADORD  DS    AL4                 LEVELS TO PRINT IN HEADER, MAX 4             
*                                                                               
HEADTLN  DS    AL1                 LENGTH OF LONGEST HEADER TITLE               
HEADALN  DS    AL1                 LENGTH OF LONGEST ACCOUNT IN HEADER          
BODYTLN  DS    AL1                 SAME AS ABOVE FOR BODY OF REPORT             
BODYALN  DS    AL1                                                              
TOTTLN   DS    AL1                 SAME AS ABOVE FOR TOTALS                     
TOTALN   DS    AL1                                                              
*                                                                               
TOTSTART DS    CL2                 MUST BE X'00'                                
TOTLEVS  DS    12CL2               ORDER REVERSED, TO PRODUCE TOTALS            
*                                  / LENGTH OF THIS DATA FOR COMPARE            
TOTNUM   DS    CL1                 NUMBER OF TOTALS NEEDED                      
*                                                                               
LISTSW   DS    CL1                 IF WE HAVE A CLIENT LIST, CHECK IT           
NEWOFF   DS    CL1                 Y, AGENCY IS ON NEW OFFICES                  
OFFSTAT  DS    CL1                 G, AGENCY USES OFFICE GROUPS                 
MASK     DS    AL1                 PRINT MASK                                   
HOURS    EQU   X'08'                                                            
AMOUNT   EQU   X'02'                                                            
MYMEND   DS    CL3                 MOS START                                    
MYMSTR   DS    CL3                 MOS END                                      
*                                                                               
MNTH1    DS    CL2                 MONTHS FOR AGEING                            
MNTH2    DS    CL2                                                              
MNTH3    DS    CL2                                                              
MNTH4    DS    CL2                                                              
*                                                                               
TIMETYPE DS    CL1                 TYPE OF TIME ON REPORT                       
SVGLIST  DS    CL15                LIST OF OFFICE GROUPS WITH SERV GRPS         
STRDATE  DS    CL3                 START DATE PACKED YMD                        
ENDDATE  DS    CL3                 END DATE PACKED YMD                          
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
CURMON   DS    CL3                 PACKED CURRENT MONTH                         
CURMONP  DS    CL23                PRINT CURRENT MONTH                          
TDATEP   DS    CL20                TRANSACTION DATE RANGE PRINT                 
FRMMONP  DS    CL6                 PRINT "FROM" MONTH                           
THRMONP  DS    CL6                 PRINT "THRU" MONTH                           
*                                                                               
DETAILPR DS    0CL27                                                            
DETAILP  DS    CL15                PRINT THE LEVEL THAT IS DETAIL               
DETAILL  DS    CL12                "LEVEL DETAIL"                               
*                                                                               
ACCSTAT  DS    CL1                                                              
GOTACC   EQU   1                                                                
*                                                                               
OPTIONS  DS    0CL16               COMPOSITE PROFILE/REQUEST OPTIONS            
OPTSJ    DS    CL1                                                              
OPT1R    DS    CL1                                                              
OPTTSK   DS    CL1                                                              
OPTSRT   DS    CL1                                                              
OPTSVGP  DS    CL1                                                              
OPTOFF   DS    CL1                                                              
OPTTIME  DS    CL1                                                              
OPTBUCK  DS    CL1                                                              
OPTTOTS  DS    CL1                                                              
         ORG   OPTIONS+L'OPTIONS                                                
*                                                                               
SAVEDATE DS    CL2                 MDATE                                        
SAVETRDT DS    CL3                 TRNSDATE                                     
SAVEHOUR DS    PL(BUCKLN)                                                       
SAVERATE DS    PL(BUCKLN)                                                       
SUMTOTAL DS    4PL(BUCKLN)                                                      
*                                                                               
CLIAFFIL DS    (NUMBUCKS)PL(BUCKLN)   NOTE THESE THREE MUST BE CONTIG.          
OFFAFFIL DS    (NUMBUCKS)PL(BUCKLN)                                             
REPAFFIL DS    (NUMBUCKS)PL(BUCKLN)                                             
*                                                                               
REQTOTS  DS    (NUMBUCKS)PL(BUCKLN)                                             
*                                                                               
PL16     DS    PL16                FOR MULTIPLYING RATE                         
*                                                                               
SRTREC   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
THISREC  DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
LISTDATA DS    (LISTDLN)C                                                       
NMPTDATA DS    (NMPTDLN)C                                                       
SAVETYPE DS    CL1                                                              
PRTSTAT  DS    CL1                                                              
SUMMARY  EQU   1                                                                
SVMOS    DS    CL2                                                              
*                                  DATA NEEDED TO BUILD A SORT KEY              
SVAREA   DS    0C                                                               
SVOFG    DS    CL12                                                             
SVSVG    DS    CL12                                                             
SVOFFS   DS    CL12                                                             
SVCLI    DS    CL12                                                             
SVPRO    DS    CL12                                                             
SVJOB    DS    CL12                                                             
SV1RLVA  DS    CL12                                                             
SV1RLVB  DS    CL12                                                             
SV1RLVC  DS    CL12                                                             
SV1RLVD  DS    CL12                                                             
SVTSK    DS    CL12                                                             
SVTRAN   DS    0CL12                                                            
SVDATE   DS    CL3                                                              
SVSUBR   DS    CL1                                                              
         ORG   SVTRAN+L'SVTRAN                                                  
SV1R     DS    CL12                                                             
SVSJ     DS    CL12                                                             
SVLEN    EQU   *-SVAREA                                                         
SVBUCKS  DS    0PL8                                                             
SVCHR    DS    PL8                 CURRENT HRS                                  
SVCAMNT  DS    PL8                 CUR AMNT                                     
SVYHR    DS    PL8                 YTD HER                                      
SVYAMNT  DS    PL8                 YTD AMOUNT                                   
SVBUCK5  DS    PL8                                                              
SVBUCK6  DS    PL8                                                              
         ORG   SVBUCKS                                                          
SVMNTH1  DS    PL8                 AGEING BUCKETS                               
SVMNTH2  DS    PL8                 "                                            
SVMNTH3  DS    PL8                 "                                            
SVMNTH4  DS    PL8                                                              
SVTOTAL  DS    PL8                                                              
         DS    PL8                                                              
*                                                                               
         ORG   SVBUCKS                                                          
SVHOUR   DS    PL8                 TIMESHEET DATA                               
SVRATE   DS    PL8                 "                                            
SVAMNT   DS    PL8                 "                                            
SVZAP    DS    PL8                 NOT USED                                     
         DS    PL8                                                              
         DS    PL8                                                              
*                                                                               
         ORG   SVBUCKS                                                          
SVBTIME  DS    PL8                                                              
SVRTIME  DS    PL8                                                              
SVNTIME  DS    PL8                                                              
SVATIME  DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
SVNUMBUK EQU   (*-SVBUCKS)/L'SVBUCKS                                            
*                                                                               
TRNHOUR  DS    PL8                                                              
TRNRATE  DS    PL8                                                              
TRNAMNT  DS    PL8                                                              
*                                                                               
PROFILES DS    0CL16                                                            
PROTIME  DS    CL1                                                              
PROSJ    DS    CL1                                                              
PRO1R    DS    CL1                                                              
PROTSK   DS    CL1                                                              
PROSRT   DS    CL1                                                              
PROSVGP  DS    CL1                                                              
PROOFF   DS    CL1                                                              
PROTOTS  DS    CL1                                                              
PROBUCK  DS    CL1                                                              
         ORG   PROFILES+L'PROFILES                                              
*                                                                               
REPSTAT  DS    CL1                                                              
CURRHRS  EQU   1                   REPORT IS CURRENT VS YTD HRS                 
TIMESHET EQU   2                   REPORT IS TIMESHEET DATA                     
*                                                                               
NMOFG    DS    CL36                                                             
NMSVG    DS    CL36                                                             
NMOFFS   DS    CL36                                                             
NMCLI    DS    CL36                                                             
NMPRO    DS    CL36                                                             
NMJOB    DS    CL36                                                             
NM1RLVA  DS    CL36                                                             
NM1RLVB  DS    CL36                                                             
NM1RLVC  DS    CL36                                                             
NM1RLVD  DS    CL36                                                             
NMTSK    DS    CL36                                                             
*                                                                               
SVCUL    DS    CL3                 COMPANY UNIT LEDGER                          
SVSTAT   DS    CL1                                                              
*                                                                               
MYKEY    DS    0CL49                                                            
MYCUL    DS    CL3                 COMPANY UNIT LEDGER                          
MYACCT   DS    CL12                ACCOUNT NUMBER                               
         ORG   MYKEY                                                            
         DS    CL49                KEY AREA                                     
*                                                                               
OFFLST   DS    CL(2*255)          OFFICE LIST STOLEN FROM ACMASTER              
REQTYPE  EQU   X'80'               REQUEST TOTAL BUFFALO RECORD                 
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                  ---- FORM A SORT ----*                       
         DS    12CL12              UP TO 12 LEVELS                              
SRTKEYLN EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTSTAT  DS    CL1                 ARRAY OF LEVELS USED IN THIS SRT REC         
PRTSVGP  EQU   X'01'               PRINT SERVICE GROUP FOR THIS RECORD          
SRTSJ    DS    CL12                SJ ACCOUNT USED FOR THIS RECORD              
SRT1R    DS    CL12                1R ACCOUNT                                   
SRTOFF   DS    CL2                 OFFICE                                       
SRTOFG   DS    CL1                 OFFICE GROUP                                 
SRTTSK   DS    CL2                 TASK                                         
SRTBUCKS DS    0C                  LOCATION OF BUCKETS                          
BUCKLN   EQU   8                   LENGTH OF BUCKETS                            
SRTBUCK1 DS    PL(BUCKLN)                                                       
SRTBUCK2 DS    PL(BUCKLN)                                                       
SRTBUCK3 DS    PL(BUCKLN)                                                       
SRTBUCK4 DS    PL(BUCKLN)                                                       
SRTBUCK5 DS    PL(BUCKLN)                                                       
SRTBUCK6 DS    PL(BUCKLN)                                                       
SBUKCONT EQU   (*-SRTBUCKS)/(BUCKLN) NUMBER OF BUCKETS                          
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
LEVLEN   EQU   12                  LENGTH OF A SORT LEVEL                       
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR TABLE, A TABLE LINLINK SORT LEVELS WITH SCONS                       
*        ADDRESSING THE DATA IN MY WORK AREA                                    
*-------------------------------------------------------------------*           
TABLED   DSECT                                                                  
TABLEV   DS    AL1                 THIS SORT LEVEL                              
TABMVC   DS    SL2                 SCON OF THIS LEVELS DATA IN ACR7D            
TABLN    DS    AL1(1)              LEN OF DATA STORED                           
TABNAME  DS    SL2                 SCON OF NAME FOR THIS LEVEL                  
TABTITLE DS    CL15                TITLE OF THIS LEVEL                          
TABBUCKS DS    6PL8                PACKED ACCUMS                                
TABPRNT  DS    AL1                 D, LEVEL IS DETAIL                           
TABTYPE  DS    AL1                 S = SJ,1=1R, T=TASK                          
TABDATA  DS    AL1                 WHERE THE DATA IS                            
TABDLEN  DS    AL1                 LENGTH OF DATA IN KEY                        
TABLST   DS    SL2                 SCON OF ADDRESS OF NAMES LIST                
TABTABLN EQU   *-TABLED                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
         DS    H                   SPACER FOR FULL ALLIGNMENT                   
MAINMAX  DS    A                                                                
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECTS FOR REQUEST OPTIONS AND PROFILE OPTIONS, THESE ARE COMPOSITED          
*-------------------------------------------------------------------*           
REQUESTD DSECT                                                                  
REQTIME  DS    CL1                                                              
REQSJ    DS    CL1                                                              
REQTSK   DS    CL1                                                              
REQ1R    DS    CL1                                                              
REQSRT   DS    CL1                                                              
REQBUCK  DS    CL1                                                              
*                                                                               
PROFKD   DSECT                         TO COVER GETPROFS KEY                    
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR THE HEADER PORTION OF BINSRCH TABLES                    *           
*-------------------------------------------------------------------*           
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   REC LENGTH                                   
BINDISPK DS    0F                  DISP TO KEY                                  
BINDISP  DS    CL1                                                              
BINKEY   DS    CL3                                                              
BINMAX   DS    F                   MAX IN THIS TABLE                            
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                TABLE DATA                                   
*                                                                               
*------------------------*                                                      
* DSECT FOR LIST TABLE                                                          
*------------------------*                                                      
LISTD    DSECT                                                                  
LISTREC  DS    0C                                                               
LISTKEY  DS    CL12                                                             
LISTNAME DS    CL36                                                             
LISTDLN  EQU   *-LISTD                                                          
*                                                                               
*------------------------*                                                      
* DSECT FOR BINSRCH NAME TABLES                                                 
*------------------------*                                                      
NMPTD    DSECT                                                                  
NMPTKEY  DS    CL12                                                             
NMPTPTR  DS    CL4                                                              
NMPTDLN  EQU   *-NMPTD                                                          
*                                                                               
*        EQUATES FOR TABLEV                                                     
TSKLEV   EQU   1                                                                
EMPLEV   EQU   2                                                                
SDELEV   EQU   3                                                                
DEPLEV   EQU   4                                                                
EOFLEV   EQU   5                                                                
JOBLEV   EQU   6                                                                
PROLEV   EQU   7                                                                
CLILEV   EQU   8                                                                
JOFLEV   EQU   9                                                                
SVGLEV   EQU   10                                                               
JOGLEV   EQU   11                                                               
REPLEV   EQU   12                                                               
EMP12LEV EQU   13                  12 BYTE EMPLOYEE LEVEL                       
*                                  FOR WHEN YOU ARE SUPRESSING HIGHER           
*                                  LEVELS OF 1R                                 
TMSLEV   EQU   14                  DUMMY TIME SHEET DATA LEVEL                  
REQLEV   EQU   255                                                              
*                                                                               
GOTSTDT  EQU   1                   USED TO BUILD TDATEP                         
GOTENDT  EQU   2                                                                
         EJECT                                                                  
*------------------------*                                                      
* DSECT FOR AGEING HEADER PRINT LINE                                            
*------------------------*                                                      
AHEADD   DSECT                                                                  
AHLINE1  DS    0CL70                                                            
AHEND    DS    CL13                                                             
         DS    CL1                                                              
AHCOL2   DS    CL13                                                             
         DS    CL1                                                              
AHCOL3   DS    CL13                                                             
         DS    CL1                                                              
AHSTART  DS    CL13                                                             
         DS    CL1                                                              
AHTOTAL  DS    CL13                                                             
         DS    CL1                                                              
AHLINE2  DS    0CL70                                                            
AHEND2   DS    CL13                                                             
         DS    CL1                                                              
AHCOL22  DS    CL13                                                             
         DS    CL1                                                              
AHCOL32  DS    CL13                                                             
         DS    CL1                                                              
AHSTART2 DS    CL13                                                             
         DS    CL1                                                              
AHTOTAL2 DS    CL13                                                             
         DS    CL1                                                              
AHLEN    EQU   *-AHLINE1                                                        
         EJECT                                                                  
ACR702   CSECT                                                                  
*                                                                               
         ENTRY RECORD                                                           
RECORD   DS    0D                  DATAMGR AREA                                 
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
*                                                                               
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
*        ACOFFALD                                                               
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096ACREPR702S05/01/02'                                      
         END                                                                    
