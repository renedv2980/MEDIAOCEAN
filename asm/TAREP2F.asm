*          DATA SET TAREP2F    AT LEVEL 064 AS OF 01/24/17                      
*PHASE T7032FC,*                                                                
*INCLUDE TALIM                                                                  
*INCLUDE TAADDCON                                                               
*INCLUDE DLFLD                                                                  
*----------------------------------------------------------------------         
* REPLACES THE ORIGINAL TWT, GHOA.PAN.LIBRARY(TAREP2FS)                         
*----------------------------------------------------------------------         
         TITLE 'T7032F - W2 TAPES, MMREF FORMAT'                                
T7032F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032F,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         BRAS  RE,MYCLEAR                                                       
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         MVC   ASPFUSER,TSPFUSER                                                
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
*                                                                               
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R1                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BRAS  RE,INITBUFF         GET & INITIALIZE BUFFER                      
         CLI   CHECKOPT,C'Y'                                                    
         BE    MODE4                                                            
*                                                                               
         MVC   ADCB,=A(W2TAPE)                                                  
         CLI   ACTEQU,ACTDOWN      ARE WE DOWNLOADING?                          
         BNE   MODE3                                                            
         MVC   ADCB,=A(TADOWN)                                                  
MODE3    BRAS  RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BRAS  RE,CLOSTAPE                                                      
*                                                                               
         CLI   ACTEQU,ACTDOWN      ARE WE DOWNLOADING?                          
         BNE   XIT                                                              
         BAS   RE,PREP2            DO DOWNLOAD DATA TO PQ                       
         B     XIT                                                              
*                                                                               
MODE4    BAS   RE,CHECKADD                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
         TM    TWAWHEN,X'04'       OVERNIGHT?                                   
         BO    VREC1                                                            
         MVI   REQSML,C'L'         SOON / LONG JOB                              
*                                                                               
VREC1    MVI   PRINTOPT,C'Y'       PRESET OPTIONS                               
         MVI   TRACOPT,C'N'                                                     
         MVI   TAPEOPT,C'Y'                                                     
         MVI   DIAGOPT,C'N'                                                     
*                                                                               
         LA    R2,SPLYEARH         YEAR                                         
         GOTO1 ANY                                                              
         MVC   YEAR,WORK                                                        
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVC   WORK(4),ALLZEROS                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),ALLZEROS                                                 
         BNE   INVXIT                                                           
         MVC   TIFYEAR(2),10(R2)                                                
*                                                                               
         LA    R2,SPLEMPH          EMPLOYER                                     
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK                                                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
*                                                                               
VREC2    LA    R2,SPLUNTH          UNIT                                         
         OC    SPLUNT,SPACES                                                    
         GOTO1 ANY                 (NOW REQUIRED)                               
         GOTO1 TAXVAL,DMCB,(3,8(R2))                                            
*                                                                               
         CLC   WORK(3),=C'PHL'                                                  
         BE    VREC3                                                            
         CLI   WORK+2,C' '         ONLY PHL, OTHER CITIES NOT ALLOWED           
         BH    CITYERR                                                          
*                                                                               
VREC3    MVC   STANAME,TGTANAME                                                 
         MVC   STACODE,TGTANUM                                                  
         MVC   TIFUNIT,WORK                                                     
         MVC   DCBSTATE,WORK                                                    
         MVC   W2STATE,WORK                                                     
*                                                                               
         MVI   TAPETYPE,NEWYORK                                                 
         MVI   SUBSOPT,C'N'                                                     
*                                                                               
         CLC   WORK(2),=C'NY'                                                   
         BE    VREC4                                                            
*                                                                               
         MVI   TAPETYPE,FEDERAL                                                 
         MVI   SUBSOPT,C'Y'                                                     
         CLC   WORK(2),=C'FD'                                                   
         BE    VREC4                                                            
*                                                                               
         MVI   SUBSOPT,C'N'                                                     
         MVI   TAPETYPE,ILLINOIS                                                
         CLC   WORK(2),=C'IL'                                                   
         BE    VREC4                                                            
         MVI   TAPETYPE,CALIF                                                   
         CLC   WORK(2),=C'CA'                                                   
         BE    VREC4                                                            
         MVI   TAPETYPE,RHODEI                                                  
         CLC   WORK(2),=C'RI'                                                   
         BE    VREC4                                                            
         MVI   TAPETYPE,CONN                                                    
         CLC   WORK(2),=C'CT'                                                   
         BE    VREC4                                                            
         MVI   TAPETYPE,GENERIC    DEFAULT TO GENERIC                           
*                                                                               
VREC4    MVC   WORK(2),SPLEMP                                                   
         MVC   WORK+2(3),SPLUNT                                                 
         BRAS  RE,EMUNCOMB                                                      
         BNE   BADCOMBO                                                         
*                                                                               
VREC8    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BRAS  RE,VOPTS                                                         
*                                                                               
         CLC   TIFUNIT,=C'PR '     IF PUERTO RICO                               
         BNE   VREC9                                                            
         OC    CTRLNUM,CTRLNUM     MUST USE CONTROL NUMBER OPTION               
         BZ    PRMISSCT                                                         
         B     VRECX                                                            
VREC9    OC    CTRLNUM,CTRLNUM     OPTION NOT ALLOWED IF NOT PR                 
         BNZ   BADOPT                                                           
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     BADXIT                                                           
*                                                                               
BADCOMBO MVC   CONHEAD(L'CMBERR),CMBERR                                         
         B     BADXIT                                                           
*                                                                               
CITYERR  MVC   CONHEAD(L'CTYERR),CTYERR                                         
         B     BADXIT                                                           
*                                                                               
PRMISSCT MVC   CONHEAD(L'PRCERR),PRCERR                                         
         B     BADXIT                                                           
*                                                                               
INVXIT   MVC   CONHEAD(L'INVERR),INVERR                                         
BADXIT   GOTO1 ERREX2                                                           
*                                                                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
INVERR   DC    C'** ERROR ** INVALID DATA'                                      
CTYERR   DC    C'** ERROR ** PHL, ONLY CITY ALLOWED'                            
CMBERR   DC    C'** INVALID EMPLOYER/UNIT COMBO **'                             
PRCERR   DC    C'** MISSING CONTROL OPTION FOR PUERTO RICO **'                  
ALLZEROS DC    C'000000000000000'             15 ZEROS                          
ALLZERO2 DC    C'000000000000000000000000000000000000000000000000' 48ZS         
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*              PRINT REPORT                                                     
*                                                                               
PREP     NTR1                                                                   
         LAY   R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         ZAP   SRTCOUNT,=P'0'                                                   
         ZAP   RWCNT,=P'0'                                                      
         MVI   FEDPRSW,0                                                        
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         ZAP   CPRWAGES,=P'0'                                                   
         ZAP   CPRTAXES,=P'0'                                                   
         ZAP   CPRFWAGE,=P'0'                                                   
         ZAP   CPRMWAGE,=P'0'                                                   
         LA    R3,FEDACCS                                                       
         BAS   RE,CLRACCS                                                       
         LA    R3,EMPACCS                                                       
         BAS   RE,CLRACCS                                                       
         LA    R3,FINACCS                                                       
         BAS   RE,CLRACCS                                                       
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(7),=C'W2 TAPE'                                           
         BAS   RE,NEEDEM           GET EMPLOYER DETAILS                         
*                                                                               
         CLC   TIFUNIT,=C'IL '   IF ILLINIOS,                                   
         BNE   *+8                                                              
         BAS   RE,PUTHDR         ADD HEADER RECORD FIRST                        
*                                                                               
         BAS   RE,PUTA                                                          
         BAS   RE,PUTE                                                          
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
*                                                                               
         OI    TIQFLAGS,TIQFDIR    WE'RE FILTERING DIRECTORY                    
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         CLC   TIFUNIT,=C'MD '     IF MARYLAND,                                 
         BNE   PREP50                                                           
         BAS   RE,PUTV             RV TOTAL RECORD                              
*                                                                               
PREP50   DS    0H                                                               
*                                                                               
         CLC   STATUNIT,=C'ME '    MAINE DOESN'T NEED RT RECORD                 
         BE    *+8                                                              
         BAS   RE,PUTT                                                          
*                                                                               
         CLI   TAPETYPE,FEDERAL    RO RECORD, NEEDS TO BE FEDERAL AND           
         BNE   PREP60                                                           
         CP    ROCNT,=P'0'         ANY RO RECORDS?                              
         BE    PREP60                                                           
         BRAS  RE,PUTU                                                          
*                                                                               
PREP60   BAS   RE,EMPTOTS          EMPLOYER TOTAL                               
*                                                                               
         CP    SRTCOUNT,=P'0'      ANY GUAM, PR OR VI TO PROCESS?               
         BE    PREP90                                                           
*                                                                               
         BAS   RE,GETSORT                                                       
         BRAS  RE,PRTOTS                                                        
*                                                                               
PREP90   BAS   RE,PUTF                                                          
         BAS   RE,FINTOTS          FINAL TOTALS                                 
*                                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*======================================================================         
PREP2    NTR1                                                                   
         BRAS  RE,NEWPRTQ2          SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R5)                                                  
*                                                                               
         L     R2,AMASTD         DO NOT PRINT LOGOS                             
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BRAS  RE,INITDWN                                                       
*                                                                               
PREP22   L     R4,AIO1                                                          
         GET   (R2),(R4)           GET FROM DISK AND PRINT                      
         L     R4,AIO1                                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,512              RECORD LENGTH IN DCBRLN                      
         D     R0,=F'40'           R1  = # OF 40 BYTE CHUNKS                    
         STC   R0,REM              REM = LEFT OVER BYTES                        
*                                                                               
         LTR   R1,R1                                                            
         BZ    PREP24                                                           
*                                                                               
PREP23   STC   R1,QUO                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R4)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,40(R4)           BUMP TO NEXT TAPE FIELD                      
         ZIC   R1,QUO                                                           
         BCT   R1,PREP23                                                        
*                                                                               
PREP24   CLI   REM,0                                                            
         BE    PREP25                                                           
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,REM                                                      
         ZIC   RE,REM                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)    PASS DATA                                    
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
PREP25   MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREP22                                                           
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   ACTEQU,ACTDOWN                                                   
         BE    NOMORE1                                                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   NOMORE2                                                          
NOMORE1  L     R1,ALOGO            NO-OP SO DON'T END AGAIN                     
         MVC   0(2,R1),=X'07FE'                                                 
         B     XIT                                                              
NOMORE2  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
GETSORT  NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
GET10    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R4,15,4(R1)         ADDRESS OF SORTREC                           
         BZ    GETXIT              ZERO, IF NO MORE                             
         MVC   GPVSTATE,0(R4)                                                   
         LA    R2,SRTRWLNQ(R4)     BUMP PAST SORTKEY                            
         CLI   FEDPRSW,0                                                        
         BNE   GET15                                                            
GET13    MVI   FEDPRSW,1                                                        
         BAS   RE,PUTE                                                          
         ZAP   RWCNT,=P'0'                                                      
         ZAP   ROCNT,=P'0'                                                      
         MVC   PRVSTATE,GPVSTATE                                                
*                                                                               
GET15    CLC   PRVSTATE,GPVSTATE                                                
         BE    GET18                                                            
         BAS   RE,PUTT                                                          
         CP    ROCNT,=P'0'         ANY RO RECORDS?                              
         BE    GET13                                                            
         BRAS  RE,PUTU                                                          
         B     GET13                                                            
*                                                                               
GET18    CLI   11(R4),1            RW                                           
         BNE   GET20                                                            
         BAS   RE,PUTTAPEW                                                      
         BRAS  RE,XTRCTRW          EXTRACT RW AMOUNTS                           
         BAS   RE,FEDTOTS                                                       
         B     GET10                                                            
*                                                                               
GET20    CLI   11(R4),2            RO                                           
         BNE   GET10                                                            
         BAS   RE,PUTTAPEO                                                      
         BRAS  RE,XTRCTRO          EXTRACT RO AMOUNTS                           
         BAS   RE,FEDTOTS                                                       
         B     GET10                                                            
*                                                                               
GETXIT   GOTO1 SORTER,DMCB,=C'END'                                              
         BAS   RE,PUTT                                                          
         CP    ROCNT,=P'0'         ANY RO RECORDS?                              
         BE    XIT                                                              
         BRAS  RE,PUTU                                                          
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOKDR                                                         
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         CLC   TISSN,SSNSTART      SKIP IF SSN LOWER THAN START SSN             
         BL    XIT                                                              
*                                                                               
         BAS   RE,FILTW2           MAY FILTER THIS RECORD                       
         BNE   XIT                                                              
*                                                                               
         BRAS  RE,GPVWRKER                                                      
         CLI   GPVOPT,C'Y'         ONLY WANT GUAM, P.R. OR V.I.                 
         BNE   IOHOOK1                                                          
         CLI   GPVWRK,C'Y'         WORKED IN GUAM, PR, OR VI?                   
         BNE   XIT                                                              
*                                                                               
IOHOOK1  AP    W2COUNT,=P'1'                                                    
         CP    W2COUNT,RECLIMIT    CHECK MAX                                    
         BH    XIT                                                              
         CP    W2COUNT,TRALIMIT                                                 
         BH    IOHOOK2                                                          
         BAS   RE,TRACEINP                                                      
*                                                                               
IOHOOK2  BAS   RE,PROCW2           PROCESS A W2 RECORD                          
         B     XIT                                                              
*                                  CHECK DIRECTORY                              
IOHOOKDR CLC   TISSN,LASTSSN       ONLY INTERESTED IN FIRST FOR SS#             
         BE    NOGOOD                                                           
         MVC   LASTSSN,TISSN                                                    
         B     ITSFINE                                                          
         EJECT                                                                  
*              CHECK ON ADDRESSES                                               
*                                                                               
CHECKADD NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LAY   R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         DROP  R5                                                               
*                                                                               
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(15),=C'ADDRESS CHECKER'                                  
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,CHEKHOOK                                                      
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         OI    TIQFLAGS,TIQFDIR    WE'RE FILTERING DIRECTORY                    
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,CHKTOTS          CHECK TOTALS                                 
         B     XIT                                                              
*                                                                               
CHEKHOOK NTR1                                                                   
         CLI   TIMODE,PROCDIR      IF THERE IS SOMETHING INTERESTING            
         BE    IOHOOKDR                                                         
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         BAS   RE,FILTW2           MAY FILTER THIS RECORD                       
         BNE   XIT                                                              
         BAS   RE,PROCW2           PROCESS A W2 RECORD                          
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A W2 RECORD                                              
*                                                                               
PROCW2   NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         BAS   RE,NEEDEM           GET EMPLOYER DETAILS                         
         BAS   RE,NEEDW4           GET W4 DETAILS                               
         L     R6,ATHISW4                                                       
         MVI   ELCODE,TAW4ELQ                                                   
         MVI   W4TYPE,0                                                         
         BAS   RE,GETEL                                                         
         BNE   *+16                                                             
         USING TAW4D,R6                                                         
         MVC   W4STA2,TAW4STA2                                                  
         MVC   W4TYPE,TAW4TYPE                                                  
*                                                                               
         CLI   CORPOPT,C'Y'        OPTION TO GET CORPS (AND FOREIGN)            
         BE    PROCW22             (FOR ADDRESS CHECKING)                       
         CLI   W4TYPE,TAW4TYFO     NOT FOR FOREIGN                              
         BE    XIT                                                              
*                                                                               
         BAS   RE,GETFED                                                        
         CLI   NEGSW,C'Y'                                                       
         BE    PROCW21                                                          
*                                                                               
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         CP    FICTAXES,=P'0'      ALLOW IF ANY FICA                            
         BNE   PROCW22                                                          
         DROP  R3                                                               
*                                                                               
         CLI   W4TYPE,TAW4TYCO     REJECT CORPS                                 
         BE    XIT                                                              
         CLI   W4TYPE,TAW4TYCA     AND CANADIANS                                
         BE    XIT                                                              
         CLI   W4TYPE,TAW4TYTR     AND TRUSTEES THAT HAVE NO FICA               
         BE    XIT                                                              
         B     PROCW22                                                          
*                                                                               
PROCW21  LA    R2,MYP1                                                          
         USING PRINTD,R2                                                        
         MVC   PRNAME,=C'*** NEGATIVE RECORD BELOW ***'                         
         BRAS  RE,SPLAT                                                         
         MVC   PRNAME,W4NAME                                                    
         MVC   PRSSN,TISSN                                                      
         LA    R3,FEDACCS                                                       
         BRAS  RE,FORMAT                                                        
         BRAS  RE,SPLAT                                                         
         BAS   RE,CLRACCS                                                       
         B     XIT                                                              
*                                                                               
PROCW22  CLI   CHECKOPT,C'Y'       CHECKING OPTION DOES OTHER STUFF             
         BE    CHECKW4                                                          
         CLI   TAPETYPE,FEDERAL    MAY NEED STATES AS WELL                      
         BE    PROCW23                                                          
         MVC   THISUNIT,W2STATE                                                 
         MVC   STATUNIT,W2STATE                                                 
         BAS   RE,GETSTATE                                                      
         CLI   NEGSW,C'Y'                                                       
         BE    PROCW21                                                          
         XC    UNITLIST,UNITLIST                                                
         MVC   UNITLIST(3),W2STATE                                              
         BRAS  RE,ADDLOCAL                                                      
         MVC   THISUNIT,UNITLIST+3                                              
         CLI   THISUNIT,0                                                       
         BE    PROCW23                                                          
         BAS   RE,GETLOCAL                                                      
         CLI   NEGSW,C'Y'                                                       
         BE    PROCW21                                                          
*                                                                               
PROCW23  LA    R3,FEDACCS          CHECK IF ANYTHING IN ACCUMULATOR             
         BAS   RE,ANYACCS                                                       
         BE    XIT                 DON'T PUT IT OUT IF ZERO                     
         BRAS  RE,CITYCHK         IF CITY, MUST HAVE WAGES OR TAXES             
         BNE   XIT                DON'T PUT IT OUT IF ZERO                      
*                                                                               
PROCW24  BAS   RE,PUTW                                                          
         CLI   TAPETYPE,FEDERAL    RO RECORD, NEEDS TO BE FEDERAL AND           
         BNE   PROCW24A                                                         
         CLI   GPVWRK,C'Y'         WORKED IN GUAM, PR, OR VI?                   
         BNE   PROCW24A                                                         
         BRAS  RE,PUTOM                                                         
****                                                                            
*        BRAS  RE,FORMAT                                                        
*        GOTO1 SPLAT                                                            
*                                                                               
PROCW24A BAS   RE,FEDTOTS                                                       
         CLI   SUBSOPT,C'Y'        OPTION TO DO SUB RECORDS                     
         BNE   PROCW26                                                          
         BAS   RE,BUILDSTA         BUILD A LIST OF STATES                       
         BRAS  RE,ADDLOCAL         ADD LOCALS TO LIST                           
         LA    R2,UNITLIST                                                      
*                                                                               
PROCW25  CLI   0(R2),0                                                          
         BE    PROCW26                                                          
         MVC   THISUNIT,0(R2)                                                   
         MVC   STATUNIT,0(R2)                                                   
         MVI   NEGSW,C'N'                                                       
         BAS   RE,GETSTATE                                                      
         MVC   THISUNIT,3(R2)                                                   
         BAS   RE,GETLOCAL                                                      
         CLI   NEGSW,C'Y'                                                       
         BE    PROCW25A                                                         
         CLC   STATUNIT,=C'PR '                                                 
         BE    *+8                                                              
         BAS   RE,PUTS                                                          
         BAS   RE,STATOTS                                                       
PROCW25A LA    R2,6(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   PROCW25                                                          
*                                                                               
PROCW26  DS    0H                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - STATE AND LOCAL LIST                       
*                                                                               
BUILDSTA NTR1                                                                   
         XC    UNITLIST,UNITLIST   RETURN VALID STATES IN UNITLIST              
         CLC   TIFUNIT(2),=C'FD'                                                
         BE    BSTA1                                                            
         MVC   UNITLIST(3),TIFUNIT ONLY 1 IF REQUESTED                          
         CLI   TIFUNIT,0                                                        
         BNE   XIT                                                              
*                                                                               
BSTA1    LA    R2,UNITLIST                                                      
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BSTA2    BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'FD '    NOT FEDERAL                                  
         BE    BSTA2                                                            
         CLC   TAW2UNIT,=C'CN '        OR CANADA                                
         BE    BSTA2                                                            
         CLI   TAW2UNIT+2,C' '         OR LOCAL                                 
         BH    BSTA2                                                            
         OC    TAW2AMTS,TAW2AMTS   SKIP IF AMOUNTS ARE 0                        
         BZ    BSTA2                                                            
         MVC   0(3,R2),TAW2UNIT                                                 
         LA    R2,6(R2)                                                         
         B     BSTA2                                                            
*                                                                               
FILTW2   NTR1                                                                   
         CLI   TIFUNIT,0           MAY BE UNIT REQUESTED                        
         BE    YES                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
FW22     BAS   RE,NEXTEL                                                        
         BNE   NO                                                               
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,TIFUNIT                                                 
         BE    YES                                                              
         B     FW22                                                             
         DROP  R6                                                               
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - DIG OUT YTD AMOUNTS                        
*                                                                               
GETFED   NTR1                                                                   
         MVI   NEGSW,C'N'                                                       
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETFED2  BAS   RE,NEXTEL                                                        
         BNE   GETFED5                                                          
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'FD '                                                 
         BNE   GETFED2                                                          
         L     R4,=A(LIMBLOCK)                                                  
         USING TMD,R4                                                           
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         L     R1,TAW2EARN         WAGES=EARNINGS+REXP                          
         CLI   TAPETYPE,FEDERAL    FOR FEDERAL                                  
         BNE   *+8                                                              
         A     R1,TAW2REXP                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    GETFED2A                                                         
         A     R1,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
GETFED2A CVD   R1,FEDWAGES                                                      
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2TAX                                                       
         CVD   R1,FEDTAXES                                                      
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2REXP                                                      
         CVD   R1,FEDREXP                                                       
         BAS   RE,NEGCHECK                                                      
         BRAS  RE,GETFICA          GET FICA AND MEDICARE WAGES/RATES            
*                                                                               
         L     R1,TAW2EARN         FICA WAGES                                   
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
         C     R1,TMBFICA                                                       
         BL    *+8                                                              
         L     R1,TMBFICA                                                       
         CVD   R1,FICWAGES                                                      
         BAS   RE,NEGCHECK                                                      
*                                                                               
         L     R1,TAW2EARN         MEDICARE WAGES                               
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
         C     R1,TMBMED                                                        
         BL    *+8                                                              
         L     R1,TMBMED                                                        
         CVD   R1,MEDWAGES                                                      
         BAS   RE,NEGCHECK                                                      
*                                                                               
*                                  COMPUTE MEDICARE                             
         M     R0,TMRMED                                                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         C     R1,TAW2FICA         (MAKE SURE NOT > TOTAL)                      
         BL    *+8                                                              
         L     R1,TAW2FICA                                                      
         CVD   R1,MEDTAXES                                                      
         BAS   RE,NEGCHECK                                                      
*                                                                               
         CP    MEDWAGES,=P'20000000'   IF MEDICARE WAGES > 200,000              
         BNH   GETFED3                                                          
         XR    R0,R0                                                            
         CVB   R1,MEDWAGES                                                      
         S     R1,=F'20000000'                                                  
         M     R0,=F'900'          2.35% - 1.45% = 0.90%                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         AP    MEDTAXES,DUB                                                     
*                                                                               
GETFED3  CVB   R0,MEDTAXES         FICA = TOTAL - MEDICARE                      
         L     R1,TAW2FICA                                                      
         SR    R1,R0                                                            
         CVD   R1,FICTAXES                                                      
         BAS   RE,NEGCHECK                                                      
*                                                                               
GETFED5  DS    0H                                                               
*&&DO                                                                           
         USING TAWSD,R6                                                         
GETFED5  ZAP   W2NHAWG,=P'0'                                                    
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GETFED9                                                          
         ICM   R1,15,TAWSNHA                                                    
         CVD   R1,W2NHAWG                                                       
         BAS   RE,NEGCHECK                                                      
*&&                                                                             
GETFED9  B     XIT                                                              
*                                                                               
         SPACE 3                                                                
NEGCHECK LTR   R1,R1                                                            
         BNMR  RE                                                               
         MVI   NEGSW,C'Y'                                                       
         BR    RE                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - DIG OUT STATE                              
*                                                                               
GETSTATE NTR1                                                                   
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         ZAP   STAWAGES,=P'0'                                                   
         ZAP   STATAXES,=P'0'                                                   
         ZAP   STASDI,=P'0'                                                     
         CLI   THISUNIT,0                                                       
         BE    XIT                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETSTA2  BAS   RE,NEXTEL                                                        
         BNE   GETSTA4                                                          
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,THISUNIT                                                
         BNE   GETSTA2                                                          
         L     R1,TAW2EARN                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
         CVD   R1,STAWAGES                                                      
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2TAX                                                       
         CVD   R1,STATAXES                                                      
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2SDI                                                       
         CVD   R1,STASDI                                                        
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2SUI                                                       
         CVD   R1,STASUI                                                        
         BAS   RE,NEGCHECK                                                      
*                                                                               
*                                  LOOK UP STATE NAME/CODE                      
GETSTA4  GOTO1 TAXVAL,DMCB,(3,THISUNIT)                                         
         MVC   STANAME,TGTANAME                                                 
         MVC   STACODE,TGTANUM                                                  
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - DIG OUT LOCAL                              
*                                                                               
GETLOCAL NTR1                                                                   
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         ZAP   LOCWAGES,=P'0'                                                   
         ZAP   LOCTAXES,=P'0'                                                   
         CLI   THISUNIT,0                                                       
         BE    XIT                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETLOC2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,THISUNIT                                                
         BNE   GETLOC2                                                          
         CLC   =C'LAX',TAW2UNIT    SKIP LAX (FAKE CA CITY)                      
         BE    GETLOC2                                                          
         L     R1,TAW2EARN                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
         CVD   R1,LOCWAGES                                                      
         BAS   RE,NEGCHECK                                                      
         L     R1,TAW2TAX                                                       
         CVD   R1,LOCTAXES                                                      
         BAS   RE,NEGCHECK                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR W2 PRINTING                              
*                                                                               
GETTID   NTR1                                                                   
         MVC   TIDID,MYSPACES      PASS TIDUNIT - RETURN TIDID                  
         BAS   RE,NEEDEX           READ EMTAX RECORD                            
         L     R6,ATHISEX                                                       
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETTID2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TATID,R6                                                         
         CLC   TATIUNIT,TIDUNIT                                                 
         BNE   GETTID2                                                          
         LA    R2,TIDID                                                         
         LA    R3,TATIID                                                        
         LA    R0,14                                                            
         MVC   TIDID,MYSPACES                                                   
*                                                                               
GETTID4  CLI   0(R3),C'A'          ONLY MOVE OUT A/N                            
         BL    GETTID6                                                          
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,1(R2)                                                         
*                                                                               
GETTID6  LA    R3,1(R3)                                                         
         BCT   R0,GETTID4                                                       
         B     XIT                                                              
*                                                                               
GETSSN   MVC   0(3,R1),THISSSN                                                  
         MVI   3(R1),C'-'                                                       
         MVC   4(2,R1),THISSSN+3                                                
         MVI   6(R1),C'-'                                                       
         MVC   7(4,R1),THISSSN+5                                                
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINES FOR ADDRESS CHECKER                                     
*                                                                               
CHECKW4  BAS   RE,ADDCON                                                        
         AP    W2COUNT,=P'1'                                                    
         OC    TADCERRS,TADCERRS   ANY ERRORS                                   
         BZ    CHECKW42                                                         
         L     R1,=A(BADACCS)                                                   
         AP    0(4,R1),=P'1'                                                    
         B     CHECKW44                                                         
*                                                                               
CHECKW42 L     R1,=A(OKACCS)       RECORD IS GOOD                               
         AP    0(4,R1),=P'1'                                                    
         CP    0(4,R1),REPLIMIT    REPORT OK ANYWAY?                            
         BNH   CHECKW44                                                         
         OC    TADCDIAG,TADCDIAG   ANY DIAGNOSTICS?                             
         BZ    XIT                                                              
         CLI   DIAGOPT,C'Y'        OPTIONALLY SHOW THESE                        
         BNE   XIT                                                              
*                                                                               
CHECKW44 LA    R2,MYP1             REPORT ON RESULTS                            
         USING PRINTD,R2                                                        
         MVC   PCKNAME,W4NAME                                                   
         MVC   PCKADD,W4ADD                                                     
         MVC   PCKSTR(30),TADCADD                                               
         LA    R2,MYP2                                                          
         MVC   PCKNAME+4(3),TISSN                                               
         MVI   PCKNAME+7,C'-'                                                   
         MVC   PCKNAME+8(2),TISSN+3                                             
         MVI   PCKNAME+10,C'-'                                                  
         MVC   PCKNAME+11(4),TISSN+5                                            
         MVC   PCKADD,W4ADD+30                                                  
         MVC   PCKSTR(30),TADCADD2                                              
         OC    PCKSTR(30),MYSPACES                                              
         LA    R2,MYP3                                                          
         CLC   TADCADD3,MYSPACES                                                
         BE    CHECKW46                                                         
         MVC   PCKSTR(30),TADCADD3                                              
         LA    R2,MYP4                                                          
*                                                                               
CHECKW46 MVC   PCKSTR(25),TADCCITY                                              
         MVC   PCKSTR+26(2),TADCSTAT                                            
         MVC   PCKSTR+29(10),TADCZIP                                            
         LA    R2,MYP3                                                          
         MVC   PCKADD,W4ADD+60                                                  
         LA    R2,MYP4                                                          
         MVC   PCKADD,W4ADD+90                                                  
         LA    R2,MYP1                                                          
         LA    R3,TADCERRS                                                      
         L     R4,=A(ERRACCS)                                                   
         LA    R0,16                                                            
*                                                                               
POSTW4   CLI   0(R3),1             WAS DIAG/ERROR SET BY TAADDCON?              
         BNE   POSTW42                                                          
         AP    0(4,R4),=P'1'       YES - ADD TO ITS COUNTER'                    
         MVC   PCKDIAG,8(R4)       AND REPORT                                   
         LA    R2,132(R2)                                                       
*                                                                               
POSTW42  LA    R3,1(R3)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,POSTW4                                                        
         BRAS  RE,SPLAT                                                         
         BRAS  RE,SPLAT                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              DIAGNOSTIC TOTALS AT END OF REPORT                               
*                                                                               
CHKTOTS  NTR1                                                                   
         BRAS  RE,SPLAT                                                         
         LA    R2,MYP1                                                          
         USING PRINTD,R2                                                        
         L     R3,=A(OKACCS)                                                    
         LA    R4,16                                                            
*                                                                               
CHKTOTS2 CP    0(4,R3),=P'0'                                                    
         BE    CHKTOTS4                                                         
         EDIT  (P4,0(R3)),(7,PCKNAME)                                           
         MVC   PCKNAME+8(24),8(R3)                                              
         BRAS  RE,SPLAT                                                         
*                                                                               
CHKTOTS4 LA    R3,32(R3)                                                        
         BCT   R4,CHKTOTS2                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINES TO GET EMTAX RECORD                                     
*                                                                               
NEEDEX   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EMTAX AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLEXD,R4                                                         
         MVI   TLEXCD,TLEXCDQ                                                   
         MVC   TLEXEMP,TIFEMP                                                   
         BAS   RE,NEEDREC                                                       
         MVC   ATHISEX,NEEDAREC    SAVE ADDRESS OF RECORD                       
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*              ROUTINES TO GET EMPLOYER DETAILS (NM, ADDR, ID#)                 
*                                                                               
NEEDEM   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,TIFEMP                                                   
         CLC   TLEMEMP,=C'TP '     IF EMPLOYER TP                               
         BNE   NEEDEM05                                                         
         CLC   YEAR,=C'2004'       AND BEFORE 2004                              
         BNL   NEEDEM05                                                         
         MVC   TLEMEMP,=C'TP1'     USE EMPLOYER TP1                             
*                                                                               
NEEDEM05 BAS   RE,NEEDREC                                                       
*                                                                               
         MVC   EMPNAME,NEEDNAME    SAVE EMPLOYER DETAILS                        
         CLC   TLEMEMP(2),=C'TP'                                                
         BNE   NEEDEM10                                                         
         CLC   YEAR,=C'2004'                                                    
         BL    NEEDEM10                                                         
         MVC   EMPNAME,=CL36'TALENT PARTNERS COMMERCIAL SERVICES'               
*                                                                               
NEEDEM10 MVC   EMPADD,MYSPACES                                                  
         MVC   EMPADD(60),NEEDADD                                               
*        CLC   TIFUNIT,=C'MD '     IF MARYLAND                                  
*        JE    NEEDEM12                                                         
*        CLC   TIFUNIT,=C'NM '     IF NEW MEXICO                                
*        JNE   NEEDEM14                                                         
NEEDEM12 MVC   EMPADD(120),NEEDADD                                              
*                                                                               
NEEDEM14 MVC   EMPFDID,MYSPACES    SAVE FEDERAL UNEMPLOYMENT ID                 
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         MVC   AIO,NEEDAREC                                                     
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BNE   XIT                                                              
         L     R6,TGELEM                                                        
         USING TATID,R6                                                         
         LA    R2,EMPFDID                                                       
         LA    R3,TATIID                                                        
         LA    R0,14                                                            
*                                                                               
NEEDEM20 CLI   0(R3),C'0'          ONLY MOVE OUT NUMERICS                       
         BL    NEEDEM30                                                         
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,1(R2)                                                         
*                                                                               
NEEDEM30 LA    R3,1(R3)                                                         
         BCT   R0,NEEDEM20                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              GET W4 DETAILS                                                   
*                                                                               
NEEDW4   NTR1                                                                   
         L     R5,TIAREC                                                        
         USING TLW2D,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,TLW2SSN                                                  
         BAS   RE,NEEDREC                                                       
         MVC   ATHISW4,NEEDAREC                                                 
         MVC   W4NAME,NEEDNAME                                                  
         MVC   W4MIDN,NEEDMIDN                                                  
         MVC   W4SUFF,NEEDSUFF                                                  
         MVC   W4ADD,NEEDADD                                                    
         MVI   NEWW4AD,C'N'        DEFAULT - NEW ADDRESS NOT FOUND              
         L     R6,ATHISW4          FIND NEW W4 ADDRESS ELEMENT                  
         USING TAA2D,R6                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   NEWW4AD,C'Y'                                                     
*                                                                               
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    *+14                                                             
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BNE   NEEDW410                                                         
         MVC   TADCADD(L'TAA2ADDR),TAA2ADDR   SET ADDRESS                       
         MVC   TADCCITY,TAA2CITY                  CITY                          
         MVC   TADCSTAT,TAA2ST                    STATE                         
         MVC   TADCZIP,TAA2ZIP                    ZIP                           
         B     NEEDW420                                                         
*                                                                               
NEEDW410 MVC   TADCADD(60),TAA2ADDR   ELSE SAVE FIRST 2 ADDRESS LINES           
         MVC   TADCADD3,MYSPACES                                                
         MVC   TADCADD3(14),TAA2CITY                                            
         MVC   TADCADD3+15(L'TAA2ST),TAA2ST                                     
         MVC   TADCADD3+18(L'TAA2ZIP),TAA2ZIP                                   
         MVC   TADCADD3+29(L'TAA2CTRY),TAA2CTRY                                 
         GOTO1 SQUASHER,DMCB,TADCADD3,L'TADCADD3                                
                                                                                
NEEDW420 XC    TADCERRS,TADCERRS   CLEAR ERRORS &                               
         XC    TADCDIAG,TADCDIAG         DIAGNOSTICS                            
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
*                                                                               
NEEDREC  NTR1                                                                   
         L     R1,ASPFUSER                                                      
         USING SPFUSERD,R1                                                      
         L     R2,ABUFFER          R2=A(BUFFER)                                 
         DROP  R1                                                               
*                                                                               
         LA    R4,12(R2)           R4=A(DATA IN BUFFER)                         
         L     R0,0(R2)            R0=A(N'ENTRIES)                              
*                                                                               
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)            BUMP TO NEXT ENTRY                           
         BCT   R0,NREC6                                                         
*                                                                               
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   XIT                                                              
*                                                                               
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
*                                                                               
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         BAS   RE,GETADD                                                        
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         EJECT                                                                  
*              ODDMENTS                                                         
*                                                                               
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         MVC   NEEDMIDN,MYSPACES                                                
         MVC   NEEDSUFF,MYSPACES                                                
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
*                                                                               
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(16),TAW4NAM1                                            
         MVC   NEEDNAME+17(16),TAW4NAM2                                         
         MVC   NEEDMIDN,TAW4MIDN                                                
         MVC   NEEDSUFF,TAW4SUFF                                                
         OC    NEEDMIDN,MYSPACES                                                
         OC    NEEDSUFF,MYSPACES                                                
         B     ITSFINE                                                          
*                                                                               
GETADD   NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TAADELQ      ADDRESS                                      
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   NEEDADD,MYSPACES                                                 
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NEEDADD(0),TAADADD                                               
         OC    NEEDADD,MYSPACES                                                 
*                                                                               
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ACCUMULATOR MAINTENANCE                                          
*                                                                               
*                                  CLEAR   R3=A(ACCUMS)                         
CLRACCS  DS    0H                                                               
         LA    R0,NACCS                                                         
         ZAP   0(L'ACCS,R3),=P'0'                                               
         LA    R3,L'ACCS(R3)                                                    
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
*                                  ANY ACTIVITY?  R3=A(ACCUMS)                  
ANYACCS  DS    0H                                                               
         LA    R0,NACCS                                                         
         LR    R1,R3                                                            
         CP    0(L'ACCS,R1),=P'0'                                               
         BNER  RE                                                               
         LA    R1,L'ACCS(R1)                                                    
         BCT   R0,*-12                                                          
         BR    RE                                                               
*                                                                               
*              ROUTINE TO ADD ACCUMS TO HIGHER LEVELS                           
*                                                                               
ADDUP    NTR1                                                                   
         LA    RE,FEDACCS          RE=A(LOWEST LEVEL OF ACCUMS)                 
         LA    R1,NACCS            R1=N'ACCUMS AT EACH LEVEL                    
*                                                                               
ADD2     LA    RF,ACCLNQ(RE)       RF=A(THIS ACCUM AT NEXT LEVEL)               
         LA    R0,NLEVELS-1        R0=N'HIGHER LEVELS OF ACCUMS                 
*                                                                               
ADD4     AP    0(L'ACCS,RF),0(L'ACCS,RE) LOWEST LVL ACCUM TO ALL HIGHER         
*                                                                               
         LA    RF,ACCLNQ(RF)       BUMP TO NEXT HIGHER LEVEL                    
         BCT   R0,ADD4                                                          
*                                                                               
         LA    RE,L'ACCS(RE)                                                    
         BCT   R1,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL OF TOTALS                                                
*                                                                               
FEDTOTS  NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         LA    R2,MYP1                                                          
         USING PRINTD,R2                                                        
         MVC   PRSSN,TISSN                                                      
         MVC   PRNAME,W4NAME                                                    
         LA    R3,FEDACCS                                                       
         BRAS  RE,FORMAT                                                        
         AP    REPCOUNT,=P'1'                                                   
         CP    REPCOUNT,REPLIMIT                                                
         BH    *+8                                                              
         BRAS  RE,SPLAT                                                         
         MVC   MYP1,MYSPACES                                                    
         BAS   RE,ADDUP                                                         
         BAS   RE,CLRACCS                                                       
         B     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
STATOTS  NTR1                                                                   
         LA    R2,MYP1                                                          
         USING PRINTD,R2                                                        
         MVC   PRNAME+20(8),STANAME                                             
         LA    R3,FEDACCS                                                       
         EDIT  STAWAGES,(12,PRWAGES),2,ZERO=BLANK                               
         EDIT  STATAXES,(12,PRTAXES),2,ZERO=BLANK                               
         EDIT  LOCWAGES,(12,PRFICWAG),2,ZERO=BLANK                              
         EDIT  LOCTAXES,(12,PRFICA),2,ZERO=BLANK                                
         EDIT  STASDI,(12,PRSDI),2,ZERO=BLANK                                   
         EDIT  STASUI,(12,PRSUI),2,ZERO=BLANK                                   
         CP    REPCOUNT,REPLIMIT                                                
         BH    *+8                                                              
         BRAS  RE,SPLAT                                                         
         MVC   MYP1,MYSPACES                                                    
         B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
EMPTOTS  NTR1                                                                   
         BRAS  RE,SPLAT                                                         
         USING PRINTD,R2                                                        
         LA    R2,MYP2                                                          
         EDIT  (P6,WCOUNT),(7,PRNAME)                                           
         MVC   PRNAME+8(9),=C'EMPLOYEES'                                        
         LA    R2,MYP1                                                          
         MVC   PRNAME,=CL30'*** EMPLOYER TOTALS ***'                            
         LA    R3,EMPACCS                                                       
         B     ALLTOTS                                                          
*                                                                               
FINTOTS  NTR1                                                                   
         BRAS  RE,SPLAT                                                         
         LA    R2,MYP1                                                          
         USING PRINTD,R2                                                        
         MVC   PRNAME,=CL30'*** FINAL TOTALS ***'                               
         LA    R3,FINACCS                                                       
         USING ACCUMD,R3                                                        
         AP    FEDWAGES,TPRWAGES                                                
         AP    FICTAXES,TPRFTAX                                                 
         AP    FICWAGES,TPRFWAGE                                                
         AP    MEDTAXES,TPRMTAX                                                 
         AP    MEDWAGES,TPRMWAGE                                                
*        AP    NHAWAGES,TPRNHAWG                                                
*                                                                               
ALLTOTS  BRAS  RE,FORMAT                                                        
         BRAS  RE,SPLAT                                                         
         BAS   RE,CLRACCS                                                       
         BRAS  RE,SPLAT                                                         
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              TAPE RECORD FORMATTING                                 *         
*---------------------------------------------------------------------*         
*                                                                               
***********************************************************************         
*        HEADER RECORD - ILLINIOS ONLY                                *         
***********************************************************************         
*                                                                               
PUTHDR   NTR1                                                                   
         L     R2,=A(FRF)                                                       
         MVC   0(12,R2),=CL12'***HEADER***'                                     
         MVC   12(9,R2),=CL9'ILEFW2***'                                         
         BAS   RE,PUTTAPE                                                       
         MVC   0(21,R2),MYSPACES                                                
         MVC   0(2,R2),=CL2'RF'         RESTORE FOR RF RECORD                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MMREF TRANSMITTER RECORD - TYPE 'A'                          *         
***********************************************************************         
*                                                                               
PUTA     NTR1                                                                   
         L     R2,=A(FRA)                                                       
         USING FRA,R2                                                           
         MVC   FRASEIN,EMPFDID     SUBMITTER'S EMPLOYER ID NUMBER (EIN)         
***      MVC   FRAPIN,=CL17'TM9VZ79P'     TALENT PARTNER PIN #                  
***      MVC   FRAPIN,=CL17'NQ52L2E6'     TALENT PARTNER PIN #                  
         MVC   FRAPIN,=CL17'YVCS9QCP'     TALENT PARTNER PIN #                  
         CLC   TIFEMP,=C'TP '                                                   
         BNE   PUTA10                                                           
         CLC   YEAR,=C'2004'                                                    
         BL    PUTA15                                                           
***      MVC   FRAPIN,=CL17'NQ52L2E6'     TALENT PARTNER COMMERCIAL SVC         
         MVC   FRAPIN,=CL17'YVCS9QCP'     TALENT PARTNER COMMERCIAL SVC         
         B     PUTA15                                                           
*                                                                               
***      MVC   FRAPIN,=CL17'NQ52L2E6'     PRINT PAYROLL PIN #                   
PUTA10   MVC   FRAPIN,=CL17'YVCS9QCP'     PRINT PAYROLL PIN #                   
PUTA15   MVC   FRARES,RESUBOPT     RESUB INDICATOR                              
         MVC   FRATLCN,TLCN        TLCN NUMBER (IF RESUB)                       
*                                                                               
         BAS   RE,ADDCONEM                                                      
*                                  COMPANY INFORMATION                          
         MVC   FRACNAM(L'EMPNAME),EMPNAME  NAME                                 
*                                                                               
         GOTOR SETADDR,DMCB,FRAQ    SET ADDRESS LINES                           
*&&DO                                                                           
         MVC   FRACLADD,TADCADD            LOCATION ADDRESS                     
         MVC   FRACDADD,TADCADD2           DELIVERY ADDRESS                     
         CLC   TIFUNIT,=C'VT '             IF VERMONT,                          
         BNE   *+10                                                             
         MVC   FRACDADD,TADCADD            DELIVERY = LOCATION                  
*&&                                                                             
         OC    FRACDADD,MYSPACES                                                
         MVC   FRACCITY,TADCCITY           CITY                                 
         MVC   FRACSTAT,TADCSTAT           STATE ABBREVIATION                   
         MVC   FRACZIP,TADCZIP             ZIP CODE                             
*        MVC   FRACZIPX,TADCZIP+6          ZIP CODE EXTENSION                   
         MVC   FRACZIPX,=C'3597'                                                
*                                                                               
*                                  SUBMITTER INFORMATION                        
         MVC   FRASNAM(L'EMPNAME),EMPNAME    NAME                               
*                                  SUBMITTER INFORMATION                        
         MVC   FRASLADD,TADCADD              LOCATION ADDRESS                   
*        CLC   TIFUNIT,=C'MD '     IF MARYLAND                                  
*        JE    PUTA22                                                           
*        CLC   TIFUNIT,=C'NM '     IF NEW MEXICO                                
*        JNE   PUTA24                                                           
PUTA22   MVC   FRASLADD,TADCADD2                                                
         MVI   FRAPCOD,C'L'                                                     
*                                  SUBMITTER INFORMATION                        
**NO-OP  MVC   FRASDADD,TADCADD2             DELIVERY ADDRESS                   
PUTA24   MVC   FRASDADD,TADCADD              DELIVERY ADDRESS                   
         OC    FRASDADD,MYSPACES                                                
         MVC   FRASCITY,TADCCITY             CITY                               
         MVC   FRASSTAT,TADCSTAT             STATE ABBREVIATION                 
         MVC   FRASZIP,TADCZIP               ZIP CODE                           
*        MVC   FRASZIPX,TADCZIP+6            ZIP CODE EXTENSION                 
         MVC   FRASZIPX,=C'3597'                                                
*                                                                               
         CLC   TIFUNIT,=C'PR '   PUERTO RICO                                    
         BNE   PUTA30                                                           
         MVI   FRANCOD,C'2'      PROBLEM CODE FOR POSTAL SERVICE                
         MVC   FRACDADD,TADCADD  DELIVERY ADDRESS                               
         MVC   FRASDADD,TADCADD  DELIVERY ADDRESS                               
*                                                                               
PUTA30   BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              BASIC INFORMATION RECORD - TYPE 'B'                    *         
*              NOT USED FOR FEDERAL OR NEW YORK                       *         
***********************************************************************         
*                                                                               
PUTB     NTR1                                                                   
         CLI   TAPETYPE,FEDERAL                                                 
         BE    XIT                                                              
*                                                                               
         CLI   TAPETYPE,NEWYORK                                                 
         BE    XIT                                                              
*                                                                               
         L     R2,=A(WRB)                                                       
         USING WRB,R2                                                           
         CLI   TAPETYPE,CALIF                                                   
         BE    XIT                                                              
         MVC   WRBYR,YEAR          YEAR                                         
         MVC   WRBEIN,EMPFDID      FEDERAL ID NUMBER                            
         MVC   WRBTNM(36),EMPNAME  EMPLOYER NAME                                
         MVC   WRBTSTR(30),EMPADD  EMPLOYER ADDRESS                             
         MVC   WRBTCITY,TADCCITY                                                
         MVC   WRBTST,TADCSTAT                                                  
         MVC   WRBTZIP,TADCZIP                                                  
         BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        MMREF EMPLOYER RECORD - TYPE 'E'                             *         
***********************************************************************         
*                                                                               
PUTE     NTR1                                                                   
         L     R2,=A(FRE)                                                       
         USING FRE,R2                                                           
         MVC   FREYR,YEAR          YEAR                                         
         MVC   FREEEIN,EMPFDID     EMPLOYER ID NUMBER (EIN)                     
*                                                                               
         BAS   RE,ADDCONEM                                                      
*                                                                               
*                                  EMPLOYER INFORMATION                         
         MVC   FREENAM(L'EMPNAME),EMPNAME   NAME                                
*                                                                               
         GOTOR SETADDR,DMCB,FREQ    SET ADDRESS LINES                           
*&&DO                                                                           
         MVC   FRELADD,TADCADD              LOCATION ADDRESS                    
         MVC   FREDADD,TADCADD2             DELIVERY ADDRESS                    
         OC    FREDADD,MYSPACES                                                 
*&&                                                                             
         MVC   FRECITY,TADCCITY             CITY                                
         MVC   FRESTAT,TADCSTAT             STATE                               
         MVC   FREZIP,TADCZIP               ZIP CODE                            
*        MVC   FREZIPX,TADCZIP+6            ZIP CODE EXTENSION                  
         MVC   FREZIPX,=C'3597'                                                 
*                                                                               
         CLC   TIFUNIT,=C'MD '                                                  
         BNE   *+8                                                              
         MVI   FREKIND,C' '                                                     
*                                                                               
         CLI   FEDPRSW,1                                                        
         BNE   PUTE05                                                           
         MVC   FRETJUR,GPVSTATE             TAX JURISDICTION                    
         CLC   GPVSTATE,=C'PR '             IF PUERTO RICO                      
         BNE   *+8                                                              
         MVI   FREKIND,C' '                                                     
*                                                                               
PUTE05   CLC   TIFUNIT,=C'PR '              IF REQUESTING PUERTO RICO           
         BNE   *+12                                                             
         MVI   FRETJUR,C'P'                                                     
         MVI   FREKIND,C' '                                                     
*                                                                               
         CLC   TIFUNIT,=C'PA '              IF PENNSYLVANIA                     
         BNE   PUTE10                                                           
*                                                                               
* as per Talent Partners, May 2014, TALENTMF-2050                               
*        MVI   FREECOD,C' '                                                     
*        MVI   FRETPSP,C' '                                                     
*                                                                               
         MVC   TIDUNIT,TIFUNIT                                                  
         BAS   RE,GETTID           READS EMTAX RECORD                           
         MVC   FREPANUM,TIDID      8 DIGIT ACCOUNT NUMBER                       
*                                                                               
PUTE10   CLC   TIFUNIT,=C'AL '     IF ALABAMA                                   
         BNE   PUTE20                                                           
         MVC   FREDADD,TADCADD     STREET ADDRESS GOES HERE                     
         MVC   TIDUNIT,TIFUNIT                                                  
         BAS   RE,GETTID           READS EMTAX RECORD                           
         MVC   FREALNUM,TIDID      10 DIGIT ACCOUNT NUMBER                      
*                                                                               
PUTE20   CLC   TIFUNIT,=C'MD '     IF MARYLAND                                  
         BNE   PUTE30                                                           
         MVI   FREECOD,C' '                                                     
         MVC   TIDUNIT,TIFUNIT                                                  
         BAS   RE,GETTID           READS EMTAX RECORD                           
*        MVC   FREMDNUM,TIDID      8 DIGIT ACCOUNT NUMBER                       
         MVC   FREDADD,TADCADD     DELIVERY ADDRESS=LOCATION ADDRESS            
         OC    FREDADD,MYSPACES                                                 
         GOTO1 DATCON,DMCB,(5,0),(20,FREDATE)   DATE                            
         LA    R6,FRETIME                                                       
         BRAS  RE,GETTMSTP         GET TIMESTAMP                                
*                                                                               
PUTE30   CLC   TIFUNIT,=C'CO '     IF COLORADO                                  
         BE    *+14                                                             
         CLC   TIFUNIT,=C'SC '     OR IF SOUTH CAROLINA                         
         BNE   PUTE40                                                           
         MVC   FREDADD,TADCADD     DELIVERY ADDRESS=LOCATION ADDRESS            
         OC    FREDADD,MYSPACES                                                 
*                                                                               
PUTE40   CLC   TIFUNIT,=C'MS '              IF MISSISSIPPI                      
         BNE   PUTE50                                                           
         CLI   TADCADD2,0                                                       
         BE    *+12                                                             
         CLI   TADCADD2,C' '                                                    
         BNE   PUTE50                                                           
         MVC   FREDADD,TADCADD              LOCATION ADDRESS                    
         OC    FREDADD,MYSPACES                                                 
*                                                                               
PUTE50   BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        MMREF EMPLOYEE WAGE RECORD - TYPE 'W'                        *         
***********************************************************************         
*                                                                               
PUTW     NTR1                                                                   
PUTWM    DS    0H                                                               
         CLI   TAPETYPE,FEDERAL    FEDERAL?                                     
         BE    PUTRWM05            YES                                          
         CLC   STATUNIT,=C'ME '    MAINE NEEDS IT                               
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'VT '    WERMONT NEEDS IT                             
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'KY '    KENTUCKY NEEDS IT                            
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'NJ '    NEW JERSEY NEEDS IT                          
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'PR '    PUERTO RICO NEEDS IT                         
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'SC '    SOUTH CAROLINA NEEDS IT                      
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'PHL'    PHILADELPHIA NEEDS IT                        
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'KS '    KANSAS NEEDS IT                              
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'UT '    UTAH NEEDS IT                                
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'MN '    MINNESOTA NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'NC '    NORTH CAROLINA NEEDS IT                      
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'DC '    DISTRICT OF COLUMBIA NEEDS IT                
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'GA '    GEORGIA NEEDS IT                             
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'MT '    MONTANA NEEDS IT                             
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'MO '    MISSOURI NEEDS IT                            
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'AR '    ARKANSAS NEEDS IT                            
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'WI '    WISCONSIN NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'OR '    OREGON    NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'IL '    ILLINOIS  NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'ID '    IDAHO     NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'NE '    NEBRASKA  NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'IN '    INDIANA   NEEDS IT                           
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'MS '    MISSISSIPPI NEEDS IT                         
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'WV '    WEST VIRGINIA NEEDS IT                       
         BE    PUTRWM05                                                         
         CLC   STATUNIT,=C'OK '    OKLAHOMA NEEDS IT                            
         BE    PUTRWM05                                                         
         B     PUTRWM75                                                         
*                                                                               
PUTRWM05 L     R2,=A(FRW)                                                       
         USING FRW,R2                                                           
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         MVC   FRWSS,TLW2SSN       SOCIAL SECURITY NUMBER                       
*                                                                               
         CLI   FRWSS,C'9'          SSN STARTING WITH 9 IS INVALID               
         BL    PUTRWM08                                                         
         MVC   FRWSS,=C'000000000'  MARK IT WITH ZEROS                          
*                                                                               
PUTRWM08 MVC   FRWEFNAM,W4NAME     EMPLOYEE FIRST NAME                          
         MVC   FRWEMIDN,W4MIDN     EMPLOYEE MIDDLE NAME                         
         MVC   FRWELNAM,W4NAME+17  EMPLOYEE LAST NAME                           
         MVC   FRWESUFF,W4SUFF     EMPLOYEE SUFFIX                              
         OC    FRWESUFF,MYSPACES                                                
*                                                                               
         AP    WCOUNT,=P'1'                                                     
         BAS   RE,ADDCON                                                        
*                                                                               
         GOTOR SETADDR,DMCB,FRWQ    SET ADDRESS LINES                           
*&&DO                                                                           
         MVC   FRWLADD,TADCADD     LOCATION ADDRESS                             
         MVC   FRWDADD,TADCADD2    DELIVERY ADDRESS                             
         OC    FRWDADD,MYSPACES                                                 
*&&                                                                             
         MVC   FRWCITY,TADCCITY    CITY                                         
         MVC   FRWSTAT,TADCSTAT    STATE                                        
         MVC   FRWZIP,TADCZIP      ZIP CODE                                     
         MVC   FRWZIPX,TADCZIP+6   ZIP CODE EXTENSION                           
         OC    FRWCITY,MYSPACES                                                 
         OC    FRWSTAT,MYSPACES                                                 
         OC    FRWZIP,MYSPACES                                                  
         OC    FRWZIPX,MYSPACES                                                 
*                                                                               
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         MVC   FRWWAGES,ALLZEROS                                                
         MVC   FRWFTAX,ALLZEROS                                                 
*                                                                               
         CLI   TAPETYPE,FEDERAL                                                 
         BNE   PUTRWM10                                                         
         CLI   GPVWRK,C'Y'         PUERTO RICO WORKER?                          
         BNE   PUTRW10A                                                         
         BRAS  RE,GPVADJW          ADJUST WAGE FOR GUAM, PR OR VI               
         B     PUTRW10A                                                         
*                                                                               
PUTRWM10 CLC   STATUNIT,=C'PR '    PUERTO RICO                                  
         BE    PUTRWM11            SKIP WAGES AND TAX                           
*                                                                               
PUTRW10A UNPK  FRWWAGES,FEDWAGES   WAGES                                        
         OI    FRWWAGES+10,X'F0'                                                
*                                                                               
         UNPK  FRWFTAX,FEDTAXES    FEDERAL TAX WITHHELD                         
         OI    FRWFTAX+10,X'F0'                                                 
*                                                                               
PUTRWM11 UNPK  FRWSSW,FICWAGES     SOCIAL SECURITY WAGES                        
         OI    FRWSSW+10,X'F0'                                                  
*                                                                               
         UNPK  FRWSSTAX,FICTAXES   SOCIAL SECURITY TAX WITHHELD                 
         OI    FRWSSTAX+10,X'F0'                                                
*                                                                               
         UNPK  FRWMCW,MEDWAGES     MEDICARE WAGES                               
         OI    FRWMCW+10,X'F0'                                                  
*                                                                               
         UNPK  FRWMTAX,MEDTAXES    MEDICARE TAX WITHHELD                        
         OI    FRWMTAX+10,X'F0'                                                 
*                                                                               
*        UNPK  FRWFRNG,FEDREXP     FRINGE BENEFITS                              
*        OI    FRWFRNG+10,X'F0'                                                 
*                                                                               
         CLC   =C'PP ',TIFEMP      EMPLOYER PP NEVER HAS PENSION                
         BE    PUTRWM50                                                         
*                                                                               
         MVI   FRWRETPL,C'1'       PENSION INDICATOR                            
         TM    W4STA2,TAW4SNPE                                                  
         BNO   *+8                                                              
*                                                                               
PUTRWM50 MVI   FRWRETPL,C'0'       OR NOT                                       
         MVI   FRWSICK,C'0'                                                     
         L     RF,=A(SRTRW)                                                     
         USING SRTRW,RF                                                         
         MVC   SRTRWST,GPVSTATE                                                 
         MVC   SRTRWSS,FRWSS                                                    
         MVI   SRTRWRC,1                                                        
         DROP  RF                                                               
*                                                                               
         CLC   TIFUNIT,=C'MS '              IF MISSISSIPPI                      
         BNE   PUTRWM52                                                         
         CLI   TADCADD2,0                                                       
         BE    *+12                                                             
         CLI   TADCADD2,C' '                                                    
         BNE   PUTRWM52                                                         
         MVC   FRWDADD,TADCADD              LOCATION ADDRESS                    
         OC    FRWDADD,MYSPACES                                                 
*                                                                               
PUTRWM52 CLI   TAPETYPE,FEDERAL    FEDERAL?                                     
         BNE   PUTRWM70                                                         
         CLI   GPVWRK,C'Y'         PR WORKER                                    
         BNE   PUTRWM70                                                         
         CP    FEDWAGES,=P'0'      NO FED OTHER THAN PR, SKIP MAIN RW           
         BE    PUTRWM55                                                         
         BAS   RE,PUTTAPEW                                                      
PUTRWM55 MVC   FRWWAGES,ALLZEROS   CHANGE FOR PUERTO RICO RW                    
         MVC   FRWFTAX,ALLZEROS                                                 
         UNPK  FRWSSW,CPRFWAGE     SOCIAL SECURITY WAGES                        
         OI    FRWSSW+10,X'F0'                                                  
                                                                                
         UNPK  FRWSSTAX,CPRFTAX    SOCIAL SECURITY TAX WITHHELD                 
         OI    FRWSSTAX+10,X'F0'                                                
                                                                                
         UNPK  FRWMCW,CPRMWAGE     MEDICARE WAGES                               
         OI    FRWMCW+10,X'F0'                                                  
                                                                                
         UNPK  FRWMTAX,CPRMTAX     MEDICARE TAX WITHHELD                        
         OI    FRWMTAX+10,X'F0'                                                 
*                                                                               
PUTRWM60 L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     RF,=A(SRTRW)                                                     
         GOTO1 SORTER,DMCB,=C'PUT',(RF)         BUT SAVE FOR RW/RO PAIR         
         AP    SRTCOUNT,=P'1'                                                   
         B     XIT                 YES, LEAVE                                   
*                                                                               
PUTRWM70 BAS   RE,PUTTAPEW                                                      
PUTRWM75 DS    0H                                                               
*&&DO                                                                           
         CLC   TIFYEAR(2),=C'10'   2010 MAY HAVE NHA EXEMPT WAGES               
         BNE   PUTRWM80                                                         
         CP    W2NHAWG,=P'0'       DO WE HAVE SOME NHA EXEMPT WAGES?            
         BNE   PUTRWM85                                                         
*&&                                                                             
PUTRWM80 CLC   STATUNIT,=C'PR '                                                 
         BNE   PUTRWM90                                                         
PUTRWM85 BRAS  RE,PUTOM                                                         
*                                                                               
PUTRWM90 BAS   RE,PUTS             NO, MAKE A STATE RECORD                      
         B     XIT                                                              
*                                                                               
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        MMREF SUPPLEMENTAL RECORD - TYPE 'RS'                                  
***********************************************************************         
*                                                                               
PUTS     NTR1                                                                   
PUTSM    DS    0H                                                               
*                                                                               
         CLI   TAPETYPE,FEDERAL    NOT NEEDED FOR FEDERAL                       
         BE    XIT                                                              
*                                                                               
         L     R2,=A(FRS)          ESTABLISH STATE SUPPLEMENTAL RECORD          
         USING FRS,R2                                                           
*                                                                               
         L     R4,TIAREC           ESTABLSIH W2 RECORD                          
         USING TLW2D,R4                                                         
*                                                                               
         MVC   FRSEFNAM(FRSNAMLN),MYSPACES  INIT EMPLOYEE NAME                  
         MVC   FRSLADD(FRSADDLN),MYSPACES   INIT EMPLOYEE ADDRESS               
         MVC   FRSOPTCD(FRSUEMLN),MYSPACES  INIT EMPLOYEE EMPLOY DATA           
*                                                                               
         BAS   RE,STATCODE         GET STAT CODE                                
*                                                                               
         USING STACDD,RF           ESTABLISH STATE DATA                         
*                                                                               
         MVC   FRSSCODE,STACDE     STATE NUMERIC CODE                           
         CLC   TIFUNIT,=C'MD '                                                  
         JNE   *+10                                                             
         MVC   FRSSCODE,MYSPACES   STATE NUMERIC CODE                           
         MVC   FRSSTCDE,STACDE                                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   FRSTXENT,MYSPACES   INIT TAXING ENTITY CODE                      
*                                                                               
         CLC   STATUNIT,=C'AZ '    ARIZONA WANTS A CODE                         
         BNE   *+10                                                             
         MVC   FRSTXENT,=C'UITAX'  INIT TAXING ENTITY CODE                      
*                                                                               
         MVC   FRSSS,TLW2SSN       SOCIAL SECURITY NUMBER                       
         MVC   FRSEFNAM,W4NAME     EMPLOYEE FIRST NAME                          
         MVC   FRSEMIDN,W4MIDN     EMPLOYEE MIDDLE NAME                         
         OC    FRSEMIDN,MYSPACES      BLANK FILL                                
*                                                                               
         MVC   FRSELNAM,W4NAME+17  EMPLOYEE LAST NAME                           
*                                                                               
         CLC   STATUNIT,=C'CA '                                                 
         BE    PUTRSM35                                                         
*                                                                               
         MVC   FRSESUFF,W4SUFF     EMPLOYEE SUFFIX                              
         OC    FRSESUFF,MYSPACES                                                
*                                                                               
         BAS   RE,ADDCON           GET EMPLOYEE'S ADDRESSES                     
*                                                                               
         GOTOR SETADDR,DMCB,FRSQ    SET ADDRESS LINES                           
*&&DO                                                                           
         CLC   STATUNIT,=C'CO '    COLOR. WANTS TO REVERSE LOC AND DEL          
         BE    PUTRSM25                                                         
         CLC   STATUNIT,=C'CT '    CONN WANTS TO REVERSE LOC AND DEL            
         BE    PUTRSM25                                                         
         CLC   STATUNIT,=C'MT '    MONTANA WANTS TO REVERSE LOC AND DEL         
         BE    PUTRSM25                                                         
         CLC   STATUNIT,=C'GA '    GEORGIA WANTS TO REVERSE LOC AND DEL         
         BNE   PUTRSM30                                                         
*                                                                               
PUTRSM25 CLC   TADCADD(3),=C'C/O'  EXCEPT FOR C/O'S                             
         BE    PUTRSM30                                                         
*                                                                               
         MVC   FRSLADD,TADCADD2    LOCATION ADDRESS                             
         MVC   FRSDADD,TADCADD     DELIVERY ADDRESS                             
PUTRSM27 OC    FRSLADD,MYSPACES                                                 
         OC    FRSDADD,MYSPACES                                                 
*                                                                               
         B     PUTRSM32                                                         
*                                                                               
PUTRSM30 DS    0H                                                               
*                                                                               
         MVC   FRSLADD,TADCADD     LOCATION ADDRESS                             
         MVC   FRSDADD,TADCADD2    DELIVERY ADDRESS                             
         OC    FRSLADD,MYSPACES                                                 
         OC    FRSDADD,MYSPACES                                                 
*&&                                                                             
PUTRSM32 MVC   FRSCITY,TADCCITY    CITY                                         
         MVC   FRSSTAT,TADCSTAT    STATE                                        
         MVC   FRSZIP,TADCZIP      ZIP CODE                                     
*                                                                               
         CLC   STATUNIT,=C'ME '    SKIP IF MAINE                                
         BE    *+10                                                             
         MVC   FRSZIPX,TADCZIP+6   ZIP CODE EXTENSION                           
*                                                                               
         CLC   STATUNIT,=C'NJ '    NJ SKIPS UNEMPLOYMENT                        
         BNE   PUTRSM35               DATA                                      
*                                                                               
         MVC   FRSOPTCD(FRSEMPAC-FRSOPTCD),MYSPACES                             
*                                                                               
         B     PUTRSM40                                                         
*                                                                               
PUTRSM35 DS    0H                                                               
*                                                                               
         CLC   STATUNIT,=C'MT '    MT & SC INIT DLR FLDS TO ZEROS               
         BE    PUTRSM36                                                         
         CLC   STATUNIT,=C'AR '    ARKANSAS                                     
         BE    PUTRSM36                                                         
         CLC   STATUNIT,=C'SC '                                                 
         BNE   PUTRSM37                                                         
*                                                                               
PUTRSM36 MVC   FRSQTOTW,ALLZEROS                                                
         MVC   FRSQTTXW,ALLZEROS                                                
*                                                                               
PUTRSM37 CLC   STATUNIT,=C'CA '    CA FORCES CODE AND                           
         BNE   PUTRSM38               REPORTING PERIOD                          
*                                                                               
         MVC   FRSOPTCD,=C'1 '                                                  
         MVC   FRSRPTPD,=C'123101'                                              
*                                                                               
         B     PUTRSM40                                                         
*                                                                               
PUTRSM38 CLC   STATUNIT,=C'MT '                                                 
         BE    PUTRSM39                                                         
         CLC   STATUNIT,=C'AZ '                                                 
         BE    PUTRSM39                                                         
         CLC   STATUNIT,=C'SC '                                                 
         BE    PUTRSM39                                                         
         CLC   STATUNIT,=C'DC '                                                 
         BNE   PUTRSM40                                                         
*                                                                               
PUTRSM39 MVC   FRSRPTPD(2),=CL2'12' REPORTING PERIOD = 12YYYY                   
         MVC   FRSRPTPD+2(4),YEAR                                               
*                                                                               
PUTRSM40 MVC   TIDUNIT,STATUNIT                                                 
*                                                                               
         BAS   RE,GETTID           READS EMTAX RECORD                           
*                                                                               
         CLC   STATUNIT,=C'GA '    GEORGIA WANTS IT IN CNTRL NUMBER             
         BE    PUTRSM42                                                         
*                                                                               
PUTRSM41 MVC   FRSEMPAC,MYSPACES                                                
*                                                                               
         CLC   STATUNIT,=C'PR '    PUERTO RICO DOES NOT NEED THIS               
         BE    PUTRSM42                                                         
         CLC   STATUNIT,=C'MD '    MARYLAND WANTS IT SOMEWHERE ELSE             
         BE    PUTRSM42                                                         
         CLC   STATUNIT,=C'IN '    INDIANA WANTS IT SOMEWHERE ELSE              
         BE    PUTRSM88                                                         
*                                                                               
         CLC   STATUNIT,=C'WI '    WISCONSIN HARD CODED                         
         BNE   *+14                                                             
         MVC   FRSEMPAC,=CL20'036102649688203' STATE EIN                        
         B     PUTRSM42                                                         
*                                                                               
         CLC   STATUNIT,=C'VT '    VERMONT HARD CODED                           
         BNE   *+14                                                             
         MVC   FRSEMPAC,=CL20'430200467282F01' STATE EIN                        
         B     PUTRSM42                                                         
*                                                                               
         CLC   STATUNIT,=C'OR '    SKIP IF NOT OREGON                           
         BE    *+10                                                             
         CLC   STATUNIT,=C'NE '    OR NEBRASKA                                  
         BE    *+10                                                             
         CLC   STATUNIT,=C'ID '    OF IDAHO                                     
         BNE   PRSM41A                                                          
*                                                                               
*       OREGON WANTS TAX ID TO BE NUMERIC, RIGHT JUSTIFIED, ZERO FILLED         
*       IDAHO  WANTS TAX ID TO BE NUMERIC, RIGHT JUSTIFIED, ZERO FILLED         
*                                                                               
         LA    RF,TIDID+L'TIDID-1        END OF TAX ID                          
         LA    R0,L'TIDID                                                       
**NO-OP  CLC   STATUNIT,=C'NE '    NEBRASKA SKIPS FIRST 2 CHARS                 
**09/12  BNE   *+8                                                              
**NO-OP  LA    R0,L'TIDID-2                                                     
         LA    RE,FRSEMPAC+L'FRSEMPAC-1  END OF RS REC TAX ID                   
         LA    R1,L'FRSEMPAC                                                    
*                                                                               
PRSM41LP DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          IF NUMERIC                                   
         BL    *+20                                                             
         MVC   0(1,RE),0(RF)          MOVE TO RECORD FIELD                      
         BCTR  RE,0                   BACK UP A BYTE                            
         BCT   R1,*+8                 DECREMENT BYTE COUNTER                    
         B     PUTRSM42                  NO MORE ROOM IN RECORD                 
*                                                                               
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   R0,PRSM41LP                                                      
*                                  END OF TAX ID                                
         MVI   FULL,C'0'           ASSUME ZERO FILLING                          
*                                  END OF TAX ID                                
         CLC   STATUNIT,=C'ID '    IF IDAHO                                     
         BNE   *+8                                                              
         MVI   FULL,C' '              SPACE FILL                                
*                                                                               
         MVC   0(1,RE),FULL        FILL                                         
         BCTR  RE,0                                                             
         BCT   R1,*-8                                                           
*                                                                               
         B     PUTRSM42                                                         
*                                                                               
PRSM41A  DS    0H                                                               
*                                                                               
         MVC   FRSEMPAC(L'TIDID),TIDID     STATE EMPLOYER NUMBER                
*                                                                               
         CLC   STATUNIT,=C'CA '                                                 
         BNE   *+14                                                             
         MVC   FRSCABRN,=C'000'                                                 
         B     *+10                                                             
*                                                                               
         CLC   STATUNIT,=C'NJ '                                                 
         BNE   *+10                                                             
         MVC   FRSSTCDE,MYSPACES                                                
*                                                                               
PUTRSM42 LA    R3,FEDACCS                                                       
         USING ACCUMD,R3           ESTABLISH STATE ACCUMULATORS                 
*                                                                               
         MVC   FRSSOTHR(FRSOTLN1),MYSPACES  INIT OTHER STATE DATA               
*                                                                               
         CLC   STATUNIT,=C'NM '    IF NEW MEXICO                                
         BNE   PRSM42AA                                                         
         CLC   =C'TP',TIFEMP                                                    
         BNE   *+10                                                             
         MVC   FRSSOTHR,=C'2968163004'                                          
         CLC   =C'PP',TIFEMP                                                    
         BNE   *+10                                                             
         MVC   FRSSOTHR,=C'3281107001'                                          
*                                                                               
PRSM42AA CLC   STATUNIT,=C'ME '    IF MAINE                                     
         BNE   *+10                                                             
         MVC   FRSSOTHR,ALLZEROS      ZERO FILL                                 
*                                                                               
         CLC   STATUNIT,=C'GA '     GEORGIA                                     
         BNE   PRSM42AB                                                         
*                                                                               
         MVC   FRSSOTHR(6),=C'12/31/'    PERIOD END DATE - MM/DD/YYYY           
         MVC   FRSSOTHR+6(4),YEAR                                               
*                                                                               
PRSM42AB CLC   STATUNIT,=C'AL '                                                 
         BNE   PRSM42A                                                          
*                                                                               
         UNPK  FRSSOTHR,FEDTAXES    FEDERAL TAX WITHHELD                        
         OI    FRSSOTHR+9,X'F0'                                                 
*                                                                               
PRSM42A  CLC   STATUNIT,=C'PHL'                                                 
         BNE   *+8                                                              
         MVI   FRSLTXTY,C'C'                                                    
*                                                                               
         CLC   STATUNIT,=C'CA '                                                 
         BNE   PUTRSM43                                                         
*                                                                               
         UNPK  FRSQTOTW,STAWAGES                                                
         OI    FRSQTOTW+10,X'F0'                                                
*                                                                               
PUTRSM43 UNPK  FRSWAGES,STAWAGES   WAGES                                        
         OI    FRSWAGES+10,X'F0'                                                
*                                                                               
         UNPK  FRSSTAX,STATAXES    STATE TAX WITHHELD                           
         OI    FRSSTAX+10,X'F0'                                                 
*                                                                               
         CLC   STATUNIT,=C'MT '    MONTANA NEEDS ZEROS                          
         BE    PUTRSM46                                                         
         CLC   STATUNIT,=C'VT '    VERMONT NEEDS ZEROS                          
         BE    PUTRSM46                                                         
         CLC   STATUNIT,=C'AR '    ARKANSAS NEEDS ZEROS                         
         BE    PUTRSM46                                                         
         CLC   STATUNIT,=C'SC '    SOUTH CAROLINA NEEDS ZEROS                   
         BNE   PUTRSM47                                                         
*                                                                               
PUTRSM46 MVC   FRSLWAGE,ALLZEROS   FOR BLANK NUMERIC FIELDS                     
         MVC   FRSLTAX,ALLZEROS                                                 
*                                                                               
         B     PUTRSM49                                                         
*                                                                               
PUTRSM47 CLC   STATUNIT,=C'KS '    KANSAS                                       
         BE    *+10                                                             
         CLC   STATUNIT,=C'CA '    CALIFORNIA                                   
         BNE   PUTRSM48                                                         
*                                                                               
         UNPK  FRSLWAGE,LOCWAGES   WAGES                                        
         OI    FRSLWAGE+10,X'F0'                                                
*                                                                               
         UNPK  FRSLTAX,LOCTAXES    LOCAL TAX WITHHELD                           
         OI    FRSLTAX+10,X'F0'                                                 
         B     PUTRSM49                                                         
*                                                                               
PUTRSM48 CLC   STATUNIT,=C'PHL'    PHILADELPHIA                                 
         BNE   PUTRSM49                                                         
*                                                                               
         UNPK  FRSLWAGE,STAWAGES   LOCAL TAXABLE WAGES                          
         OI    FRSLWAGE+10,X'F0'                                                
         MVC   FRSSTAX,=C'00000000000' STATE TAX. WAGES = 0 FOR CITY            
*                                                                               
         UNPK  FRSLTAX,STATAXES    LOCAL TAX WITHHELD                           
         OI    FRSLTAX+10,X'F0'                                                 
*                                                                               
PUTRSM49 MVC   FRSSUP1,MYSPACES    SUPPLEMENTAL INFO                            
*                                                                               
         CLC   STATUNIT,=C'KY '                                                 
         BNE   PUTRSM50                                                         
*                                                                               
         MVC   FRSSUPKR,=C'00000000000'                                         
         MVC   FRSSUPKJ,=C'00000000000'                                         
         MVC   FRSSUPKI,=C'00000000000'                                         
         MVC   FRSSUPKD,=C'00000000000'                                         
*                                                                               
PUTRSM50 CLC   STATUNIT,=C'KS '                                                 
         BNE   PUTRSM55                                                         
         MVC   FRSSUPKS,ALLZEROS                                                
*                                                                               
PUTRSM55 MVC   FRSSUP2,MYSPACES                                                 
*                                                                               
         CLC   STATUNIT,=C'GA '    GEORGIA TAX ID IN CNTRL NUMBER               
         BNE   *+10                                                             
         MVC   FRSCNTRL(9),TIDID                                                
*                                                                               
         CLC   STATUNIT,=C'PHL'    PHILADELPHIA BUSINESS TAX ACCOUNT            
         BNE   *+10                NUMBER IN CNTRL NUMBER                       
         MVC   FRSCNTRL,=C'8045387'                                             
*                                                                               
         CLC   STATUNIT,=C'NJ '    NEW JERSEY                                   
         BNE   PUTRSM60                                                         
*                                                                               
         UNPK  FRSSUPUI,STASUI     UNEMPLOYMENT INSURANCE                       
         OI    FRSSUPUI+4,X'F0'                                                 
         UNPK  FRSSUPDI,STASDI     DISABILITY INSURANCE                         
         OI    FRSSUPDI+4,X'F0'                                                 
*                                                                               
         TM    W4STA2,TAW4SNPE                                                  
         BZ    *+8                                                              
         MVI   FRSSUPPI,C'P'                                                    
*                                                                               
         MVC   FRSSUPDC,ALLZEROS   DEFRERRED COMPENSATION AMOUNT                
*                                                                               
         USING TAW2D,R6                                                         
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PUTRSM58 BAS   RE,NEXTEL                                                        
         BNE   PUTRSM92                                                         
         CLC   TAW2UNIT,=C'NJ '    GET NJ FLI                                   
         BNE   PUTRSM58                                                         
         EDIT  TAW2SFLI,(5,FRSFLI),0,ZERO=NOBLANK,FILL=0                        
         B     PUTRSM92                                                         
         DROP  R6                                                               
*                                                                               
PUTRSM60 CLC   STATUNIT,=C'MA '    MASSACHUSETTS                                
         BNE   PUTRSM65                                                         
         ZAP   DUB,FICTAXES        WANTS FICA + MEDICARE TAXES                  
         AP    DUB,MEDTAXES                                                     
         UNPK  FRSSUPFM,DUB                                                     
         OI    FRSSUPFM+10,X'F0'                                                
         B     PUTRSM92                                                         
*                                                                               
PUTRSM65 CLC   STATUNIT,=C'PR '    PUERTO RICO                                  
         BNE   PUTRSM70                                                         
         MVC   FRSTXENT,ALLZERO2   ZEROS                                        
         MVC   FRSSTCDE,MYSPACES   SPACES                                       
         MVC   FRSPRZR,ALLZERO2    ZEROS                                        
         MVC   FRSPRZR2,ALLZERO2   ZEROS                                        
         MVC   FRSPRZR3,ALLZERO2   ZEROS                                        
         MVC   FRSSPRZ,ALLZERO2    ZEROS                                        
         MVC   FRSSPRZ2,ALLZERO2   ZEROS                                        
         MVC   FRSPREPN,MYSPACES   SPACES (AS OF 2007)                          
         MVC   FRSPRCOD,ALLZERO2   ZEROS                                        
         MVC   FRSESC,MYSPACES     SPACES                                       
*                                                                               
         ZAP   DUB,CTRLNUM         CONTROL NUMBER                               
         UNPK  FRSSCNTL,DUB                                                     
         OI    FRSSCNTL+8,X'F0'                                                 
         AP    CTRLNUM,=P'1'       INCREMENT CONTROL NUMBER BY 1                
*                                                                               
         MVC   FRSSACCD,MYSPACES   SPACES (AS OF 2007)                          
         B     PUTRSM92                                                         
*                                                                               
PUTRSM70 CLC   STATUNIT,=C'MD '    MARYLAND                                     
         BNE   PUTRSM80                                                         
         MVC   FRSSOTHR,ALLZEROS                                                
         MVC   FRSMDAN,TIDID       TAX ID ACCOUNT NUMBER                        
         MVC   FRSMDEIN,EMPFDID    EMPLOYER ID NUMBER                           
*                                                                               
         UNPK  FRSFDWG,FEDWAGES    FEDERAL WAGES                                
         OI    FRSFDWG+10,X'F0'                                                 
*                                                                               
         UNPK  FRSFDTX,FEDTAXES    FEDERAL TAX WITHHELD                         
         OI    FRSFDTX+10,X'F0'                                                 
*                                                                               
         L     R6,ATHISW4                                                       
         USING TAWHD,R6                                                         
         MVI   ELCODE,TAWHELQ      WITHHOLDING ELEMENT                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PUTRSM75 BAS   RE,NEXTEL                                                        
         BNE   PUTRSM78                                                         
         CLC   TAWHUNIT,=C'FD '    GET FEDERAL EXEMPTIONS                       
         BNE   PUTRSM75                                                         
         EDIT  (B1,TAWHEXS),(2,FRSFDEX),ZERO=NOBLANK,FILL=0                     
         DROP  R6                                                               
*                                                                               
PUTRSM78 GOTO1 DATCON,DMCB,(5,0),(20,FRSDATE)   DATE                            
         LA    R6,FRSTIME                                                       
         BRAS  RE,GETTMSTP         GET TIMESTAMP                                
         B     PUTRSM92                                                         
*                                                                               
PUTRSM80 CLC   STATUNIT,=C'AL '    ALABAMA                                      
         BNE   PUTRSM82                                                         
         MVC   FRSFEIN,EMPFDID                                                  
         MVC   FRSSUMSI,ALLZEROS   MISC INCOME (1099)                           
         MVC   FRSSYEAR,YEAR       PAYMENT YEAR                                 
         B     PUTRSM92                                                         
*                                                                               
PUTRSM82 DS    0H                                                               
*                                                                               
*        ARKANSAS                                                               
*                                                                               
         CLC   STATUNIT,=C'AR '    ARKANSAS                                     
         BNE   PUTRSM86                                                         
*                                                                               
         MVC   FRSAFEIN,EMPFDID    TAXID                                        
*                                                                               
         MVC   FRSSUP2,MYSPACES    INIT STATE SUPPLEMENTAL AREA 2               
*                                                                               
*                                                                               
         LA    RF,FRSARID          ARKANSAS ID AREA                             
         LA    RE,TIDID            TAX ID                                       
         LA    R0,L'TIDID          LENGTH OF TAX ID                             
*                                                                               
PUTRSMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C'-'                                                       
         BE    PUTRSMCN                                                         
*                                                                               
         CLI   0(RE),C'0'          MOVE OUT NUMERICS AND CHARACTERS             
         BL    *+16                                                             
         CLI   0(RE),C'9'                                                       
         BH    PUTRSMCN                                                         
         B     PUTRSM84                                                         
*                                                                               
         CLI   0(RE),C'A'                                                       
         BL    *+16                                                             
         CLI   0(RE),C'Z'                                                       
         BH    PUTRSMCN                                                         
         B     PUTRSM84                                                         
*                                                                               
         CLI   0(RE),C'a'                                                       
         BL    *+12                                                             
         CLI   0(RE),C'z'                                                       
         BH    PUTRSMCN                                                         
*                                                                               
PUTRSM84 MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
*                                                                               
PUTRSMCN LA    RE,1(RE)                                                         
         BCT   R0,PUTRSMLP                                                      
*                                                                               
PUTRSMDN DS    0H                                                               
*                                                                               
PUTRSM86 DS    0H                                                               
*                                                                               
*        OREGON                                                                 
*                                                                               
         CLC   STATUNIT,=C'OR '    SKIP IF NOT OREGON                           
         BNE   PUTSRM87                                                         
*                                                                               
         MVC   FRSORAMW,ALLZEROS   AMTRACK WAGES                                
         MVC   FRSORWWW,ALLZEROS   WATERWAY WAGES                               
         MVC   FRSORHDW,ALLZEROS   HYDROELECTRIC DAM WAGES                      
         MVC   FRSORIIB,ALLZEROS   IMPUTED INCOME BENEFITS                      
         MVC   FRSORIIT,ALLZEROS   IMPUTED INCOME TUITION                       
*                                                                               
PUTSRM87 CLC   STATUNIT,=C'AZ '    SKIP IF NOT ARIZONA                          
         BNE   PUTRSM88                                                         
         UNPK  FRSQTOTW,STAWAGES   STATE TOTAL WAGES                            
         OI    FRSQTOTW+10,X'F0'                                                
*                                                                               
         UNPK  FRSQTTXW,STATAXES    STATE TAX WITHHELD                          
         OI    FRSQTTXW+10,X'F0'                                                
*                                                                               
         MVC   FRSSTCDE,MYSPACES                                                
         MVC   FRSWAGES,MYSPACES                                                
         MVC   FRSSTAX,MYSPACES                                                 
*                                                                               
PUTRSM88 CLC   STATUNIT,=C'IN '    SKIP IF NOT INDIANA                          
         BNE   PUTRSM90                                                         
         MVC   FRSBLNK,MYSPACES                                                 
*                                                                               
         USING STACDD,RF                                                        
         MVC   FRSSTCD2,STACDE     STATE NUMERIC CODE                           
         DROP  RF                                                               
*                                                                               
         UNPK  FRSSWAG,STAWAGES    STATE TOTAL WAGES                            
         OI    FRSSWAG+10,X'F0'                                                 
         UNPK  FRSSTAX2,STATAXES   STATE TAX WITHHELD                           
         OI    FRSSTAX2+10,X'F0'                                                
         MVC   FRSBLNK2,MYSPACES                                                
         MVC   FRSZEROS,ALLZEROS                                                
         MVC   FRSETID,TIDID       EMPLOYER TAXPAYER ID                         
         MVC   FRSETLOC,=C'001'    EMPLOYER TID LOCATION                        
*                                                                               
PUTRSM90 CLC   STATUNIT,=C'MS '             IF MISSISSIPPI                      
         BNE   PUTRSM92                                                         
         MVC   FRSZEROS,ALLZEROS                                                
         CLI   TADCADD2,0                                                       
         BE    *+12                                                             
         CLI   TADCADD2,C' '                                                    
         BNE   PUTRSM92                                                         
         MVC   FRSDADD,TADCADD              LOCATION ADDRESS                    
         OC    FRSDADD,MYSPACES                                                 
*                                                                               
PUTRSM92 BAS   RE,PUTTAPES                                                      
*                                                                               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              STATE TOTAL RECORD - TYPE 'RST'                                  
*              ONLY FOR CALIFORNIA MMREF                                        
***********************************************************************         
*&&DO                                                                           
PUTRST   NTR1                                                                   
         LA    R3,EMPACCS                                                       
         USING ACCUMD,R3                                                        
         L     R2,=A(FRST)                                                      
         USING FRST,R2                                                          
         BAS   RE,STATCODE                                                      
*                                                                               
         USING STACDD,RF                                                        
         MVC   FRSTSTCDE,STACDE     STATE NUMERIC CODE                          
         DROP  RF                                                               
*                                                                               
         UNPK  FRSTNEMP,TAPSCNT     NUMBER OF RS RECORDS                        
         OI    FRSTNEMP+6,X'F0'                                                 
*                                                                               
         UNPK  FRSTUITO,STAWAGES    STATE WAGES                                 
         OI    FRSTUITO+10,X'F0'                                                
*                                                                               
         UNPK  FRSTITWT,STAWAGES                                                
         OI    FRSTITWT+10,X'F0'                                                
*                                                                               
         UNPK  FRSTITWH,STATAXES    STATE TAXES                                 
         OI    FRSTITWH+10,X'F0'                                                
*                                                                               
         MVC   FRSTM1EM,=CL7'0000000'                                           
         MVC   FRSTM2EM,=CL7'0000000'                                           
         MVC   FRSTM3EM,=CL7'0000000'                                           
*                                                                               
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*              STATE CODE ROUTINE                                               
*              FINDS THE STATE CODE                                             
***********************************************************************         
         USING STACDD,RF                                                        
STATCODE LAY   RF,STATECDE         POINT TO STATE CODES TABLE                   
STATCDE1 CLC   =X'FFFFFF',STASTA   EOT                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF CAN'T FIND STATE                      
         CLC   STATUNIT,STASTA     COMPARE STATES                               
         BE    STATCDEX                                                         
         LA    RF,STACLNQ(RF)      BUMP TO NEXT STATE                           
         B     STATCDE1                                                         
*                                                                               
STATCDEX BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*        MMREF TOTAL RECORD - TYPE 'T'                                *         
***********************************************************************         
*                                                                               
PUTT     NTR1                                                                   
PUTTM    DS    0H                                                               
         LA    R3,EMPACCS                                                       
         USING ACCUMD,R3                                                        
         L     R2,=A(FRT)                                                       
         USING FRT,R2                                                           
         UNPK  FRTNRW,RWCNT        NUMBER OF RW RECORDS                         
         CLI   TAPETYPE,FEDERAL                                                 
         BE    PUTT5                                                            
         UNPK  FRTNRW,TAPSCNT      NUMBER OF RS RECORDS                         
         CLC   STATUNIT,=C'MA '    MASS WANTS STATE WAGES AND TAX               
         BNE   PUTT5                                                            
         OI    FRTNRW+6,X'F0'                                                   
         UNPK  FRTWAGES,STAWAGES   STATE WAGES                                  
         OI    FRTWAGES+14,X'F0'                                                
         UNPK  FRTFTAX,STATAXES    STATE TAX WITHHELD                           
         OI    FRTFTAX+14,X'F0'                                                 
         MVC   FRTSSW(45),MYSPACES                                              
         ZAP   DUB,FICTAXES        FICA TAX WITHHELD                            
         AP    DUB,MEDTAXES                                                     
         UNPK  FRTMTAX,DUB                                                      
         OI    FRTMTAX+14,X'F0'                                                 
         MVI   FRTSSTIP,C'0'                                                    
         MVC   FRTSSTIP+1(255),FRTSSTIP                                         
         B     PUTT7                                                            
*                                                                               
PUTT5    OI    FRTNRW+6,X'F0'                                                   
*                                                                               
         CLC   STATUNIT,=C'CT '    CONNECTICUT  WANTS STATE WAGE&TAX            
         BNE   PUTT5B                                                           
*        BNE   PUTT5A                                                           
*                                                                               
         UNPK  FRTWAGES,STAWAGES   STATE WAGES                                  
         OI    FRTWAGES+14,X'F0'                                                
*                                                                               
         UNPK  FRTFTAX,STATAXES    STATE TAX WITHHELD                           
         OI    FRTFTAX+14,X'F0'                                                 
*                                                                               
         MVC   FRTSSW(60),MYSPACES                                              
         MVC   FRTSSTIP(120),MYSPACES                                           
         MVC   FRTBQSC(75),MYSPACES                                             
*        MVC   FRTFRNG,MYSPACES                                                 
         MVC   FRTELIFE(90),MYSPACES                                            
*                                                                               
         B     PUTT10                                                           
*&&DO                                                                           
PUTT5A   MVC   FRTFRNG,MYSPACES                                                 
         CLC   TIFUNIT,=C'FD '                                                  
         BE    PUTT5B                                                           
         CLC   TIFUNIT,=C'PR '                                                  
         BE    *+10                                                             
*&&                                                                             
PUTT5B   MVC   FRTFRNG,ALLZEROS                                                 
         MVC   FRTWAGES,ALLZEROS                                                
         MVC   FRTFTAX,ALLZEROS                                                 
*                                                                               
         CLC   TIFUNIT,=C'MD '     IF MARYLAND                                  
         BNE   *+8                                                              
         MVI   FRTSSA+L'FRTSSA,C' '                                             
*                                                                               
         CLI   FEDPRSW,1                                                        
         BE    PUTT5X                                                           
         CLC   TIFUNIT,=C'PR '                                                  
         BE    PUTT5S                                                           
         UNPK  FRTWAGES,FEDWAGES   FEDERAL WAGES                                
         UNPK  FRTFTAX,FEDTAXES    FEDERAL TAX WITHHELD                         
*                                                                               
PUTT5S   UNPK  FRTSSW,FICWAGES     SOCIAL SECURITY WAGES                        
         UNPK  FRTSSTAX,FICTAXES   SOCIAL SECURITY TAX WITHHELD                 
         UNPK  FRTMCW,MEDWAGES     MEDICARE WAGES                               
         UNPK  FRTMTAX,MEDTAXES    MEDICARE TAX WITHHELD                        
         B     PUTT6                                                            
*                                                                               
PUTT5X   UNPK  FRTSSW,TPRFWAGE     SOCIAL SECURITY WAGES                        
         UNPK  FRTSSTAX,TPRFTAX    SOCIAL SECURITY TAX WITHHELD                 
         UNPK  FRTMCW,TPRMWAGE     MEDICARE WAGES                               
         UNPK  FRTMTAX,TPRMTAX     MEDICARE TAX WITHHELD                        
*                                                                               
PUTT6    OI    FRTWAGES+14,X'F0'                                                
         OI    FRTFTAX+14,X'F0'                                                 
         OI    FRTSSW+14,X'F0'                                                  
         OI    FRTSSTAX+14,X'F0'                                                
         OI    FRTMCW+14,X'F0'                                                  
         OI    FRTMTAX+14,X'F0'                                                 
*                                                                               
***      CLC   TIFUNIT,=C'PR '     IF PUERTO RICO,                              
***      BNE   PUTT7                                                            
***      MVC   FRTZERO1,ALLZERO2   FILL WITH ZEROS                              
***      MVC   FRTZERO2,ALLZERO2   FILL WITH ZEROS                              
*                                                                               
PUTT7    CLC   TIFUNIT,=C'PA '     IF PENNSYLVANIA,                             
         BNE   PUTT8                                                            
         MVC   FRTPASP,MYSPACES                                                 
         UNPK  FRTPARSN,TAPSCNT     NUMBER OF RS RECORDS                        
         OI    FRTPARSN+6,X'F0'                                                 
         UNPK  FRTPAWAG,STAWAGES    STATE WAGES                                 
         OI    FRTPAWAG+14,X'F0'                                                
         UNPK  FRTPATAX,STATAXES    STATE TAX WITHHELD                          
         OI    FRTPATAX+14,X'F0'                                                
*                                                                               
PUTT8    CLC   TIFUNIT,=C'MS '     IF MISSISSIPPI,                              
         BNE   PUTT9                                                            
         MVC   FRTLCLWA,ALLZERO2                                                
*                                                                               
PUTT9    CLC   TIFUNIT,=C'DC '     IF DISTRICT OF COLUMBIA                      
         BNE   PUTT10                                                           
         MVC   FRTDCEHC,ALLZEROS                                                
*                                                                               
*                                                                               
PUTT10   BAS   RE,PUTTAPE                                                       
*                                                                               
         CLI   TAPETYPE,FEDERAL                                                 
         BE    XIT                                                              
*                                                                               
         CLC   STATUNIT,=C'MT '    MONTANA  NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'IL '    ILLINOIS NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'ID '    IDAHO    NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'NE '    NEBRASKA NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'IN '    INDIANA  NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'OR '    OREGON   NEEDS RV RECORD                     
         BE    *+10                                                             
         CLC   STATUNIT,=C'OK '    OKLAHOMA NEEDS RV RECORD                     
         BNE   PUTT20                                                           
*                                                                               
         BAS   RE,PUTV                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
PUTT20   CLC   STATUNIT,=C'PR '    PUERTO RICO NEEDS RU RECORD                  
         BNE   XIT                                                              
         CP    ROCNT,=P'0'         ANY RO RECORDS?                              
         BE    PUTT30                                                           
         BRAS  RE,PUTU                                                          
PUTT30   BAS   RE,PUTV             PUERTO RICO NEEDS RV RECORD                  
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        MMREF TOTAL RECORD - TYPE 'V'                                *         
***********************************************************************         
*                                                                               
PUTV     NTR1                                                                   
         L     R2,=A(FRV)                                                       
         CLC   TIFUNIT,=C'PR '     IF PUERTO RICO, SKIP STATCODE                
         BE    PUTV50                                                           
         CLC   TIFUNIT,=C'IL '     IF ILLINOIS     SKIP STATCODE                
         BE    PUTV50                                                           
         CLC   TIFUNIT,=C'OR '     IF OREGON       SKIP STATCODE                
         BE    PUTV50                                                           
         CLC   TIFUNIT,=C'NE '     IF NEBRASKA     SKIP STATCODE                
         BE    PUTV50                                                           
         BAS   RE,ADDCONEM                                                      
         BAS   RE,STATCODE                                                      
PUTV50   BRAS  RE,PUTRV                                                         
         BAS   RE,PUTTAPE                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MMREF FINAL RECORD - TYPE 'F'                                *         
***********************************************************************         
*              ROUTINE TO WRITE FINAL RECORD (TYPE 'F')                         
*                                                                               
PUTF     NTR1                                                                   
PUTFM    DS    0H                                                               
         L     R2,=A(FRF)                                                       
         USING FRF,R2                                                           
*                                                                               
         MVC   FRFNRW,MYSPACES                                                  
         CLC   TIFUNIT,=C'PA '     PENNSYLVANIA                                 
         BE    PUTF10                                                           
         CLC   TIFUNIT,=C'CT '     CONNECTICUT                                  
         BE    PUTF20                                                           
         CLC   TIFUNIT,=C'AZ '     ARIZONA                                      
         BE    PUTFX                                                            
*                                                                               
         UNPK  FRFNRWKY,TAPSCNT    KENTUCKY IS 6 CHARS LONG                     
         OI    FRFNRWKY+5,X'F0'                                                 
*                                                                               
         CLC   TIFUNIT,=C'KY '                                                  
         BE    PUTFX                                                            
*                                                                               
         UNPK  FRFNRW,TAPWCNT      NUMBER OF RW RECORDS                         
*                                                                               
PUTF05   DS    0H                                                               
*                                                                               
         CLI   TAPETYPE,FEDERAL                                                 
         BE    *+10                                                             
         UNPK  FRFNRW,TAPSCNT      NUMBER OF RS RECORDS                         
*                                                                               
         OI    FRFNRW+8,X'F0'                                                   
*                                                                               
         B     PUTFX                                                            
*                                                                               
PUTF10   UNPK  FRFRSN,TAPSCNT      NUMBER OF RS RECORDS                         
         OI    FRFRSN+6,X'F0'                                                   
*                                                                               
         CLC   TIFUNIT,=C'RI '     RHODE ISLAND - DONE                          
         BE    PUTFX                                                            
*                                                                               
         LA    R3,FINACCS                                                       
         USING ACCUMD,R3                                                        
         UNPK  FRFWAGE,STAWAGES    STATE WAGES                                  
         OI    FRFWAGE+14,X'F0'                                                 
         UNPK  FRFTAX,STATAXES     STATE TAX WITHHELD                           
         OI    FRFTAX+14,X'F0'                                                  
         B     PUTFX                                                            
         DROP  R3                                                               
*                                                                               
*                                  CONNECTICUT                                  
PUTF20   UNPK  FRFRSN2,TAPSCNT     NUMBER OF RS RECORDS                         
         OI    FRFRSN2+8,X'F0'                                                  
*                                                                               
         LA    R3,FINACCS                                                       
         USING ACCUMD,R3                                                        
*                                                                               
         UNPK  FRFWAGE2,STAWAGES    STATE WAGES                                 
         OI    FRFWAGE2+15,X'F0'                                                
*                                                                               
         UNPK  FRFTAX2,STATAXES     STATE TAX WITHHELD                          
         OI    FRFTAX2+15,X'F0'                                                 
*                                                                               
         B     PUTFX                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
PUTFX    BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*             TAADDCON PROCESSING                                     *         
***********************************************************************         
*                                                                               
ADDCON   NTR1                                                                   
         CLI   NEWW4AD,C'Y'        IF NEW ADDRESS FOUND                         
         BE    XIT                 DON'T GO TO ADDCON                           
         LA    R1,W4ADD                                                         
         MVC   TADCSTA2,W4STA2     PASS W4 STATUS (FOREIGN ETC)                 
         B     ADDCON2                                                          
*                                                                               
ADDCONEM NTR1                                                                   
         LA    R1,EMPADD                                                        
         MVI   TADCSTA2,0                                                       
*                                                                               
ADDCON2  ST    R1,TADCAADD         SET UP FOR ADDCON                            
         MVC   TADCASQ,SQUASHER                                                 
         MVC   TADCACHP,CHOPPER                                                 
         GOTO1 =V(TAADDCON),DMCB,ADDCOND                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              TAPE SUPPORT                                           *         
***********************************************************************         
*                                                                               
PUTTAPE  NTR1                                                                   
         CLI   TRACOPT,C'Y'        MOST RECORDS                                 
         BNE   PUTTAPE2                                                         
         B     PUTTAPEX                                                         
*                                                                               
PUTTAPEW NTR1                      W RECORDS                                    
         AP    RWCNT,=P'1'                                                      
         AP    TAPWCNT,=P'1'                                                    
         B     PUTTAPET                                                         
*                                                                               
PUTTAPEO NTR1                      O RECORDS                                    
         AP    ROCNT,=P'1'                                                      
         AP    TAPOCNT,=P'1'                                                    
         B     PUTTAPET                                                         
*                                                                               
PUTTAPES NTR1                      S RECORDS                                    
         AP    TAPSCNT,=P'1'                                                    
*                                                                               
PUTTAPET CLI   TRACOPT,C'Y'                                                     
         BNE   PUTTAPE2                                                         
         CP    WCOUNT,TRALIMIT     AND TYPE W AND SUBSIDS                       
         BH    PUTTAPE2            TRACE THE FIRST N TYPE W'S                   
*                                                                               
PUTTAPEX MVC   MYP(100),NUMBERS                                                 
         MVC   MYP2(100),000(R2)   128 BYTES FOR NY                             
*        MVC   MYP3(28),100(R2)                                                 
*        CLI   TAPETYPE,NEWYORK                                                 
*        BE    PUTTAPEY                                                         
         MVC   MYP3(100),100(R2)                                                
*        MVC   MYP4(075),200(R2)   ELSE 275                                     
*        CLI   TAPETYPE,FEDERAL                                                 
*        BNE   PUTTAPEY            EXCEPT FOR FEDERAL                           
         MVC   MYP4(100),200(R2)   THEN 512                                     
         MVC   MYP5(100),300(R2)                                                
         MVC   MYP6(100),400(R2)                                                
         MVC   MYP7(12),500(R2)                                                 
*                                                                               
PUTTAPEY BRAS  RE,SPLAT                                                         
         BRAS  RE,SPLAT                                                         
*                                                                               
PUTTAPE2 CLI   ACTEQU,ACTDOWN                                                   
         BE    *+8                                                              
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R1,ADCB                                                          
         PUT   (1),(2)             (R2)=A(FROM AREA)                            
         B     XIT                                                              
*                                                                               
NUMBERS  DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         DC    C'1234567890'                                                    
         EJECT                                                                  
*              TRACING ROUTINES                                                 
*                                                                               
TRACEINP NTR1                                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'W2 RECORD'                                         
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
*                                                                               
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BRAS  RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
*                                                                               
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
         BRAS  RE,SPLAT                                                         
         BRAS  RE,SPLAT                                                         
         B     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ODD ROUTINES                                                     
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
*                                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=524'                                   
         EJECT                                                                  
         DS    0F                                                               
LBUFFER  DC    F'600016'           (150*4000 + 16)                              
TRACOUNT DC    PL6'0'                                                           
W2COUNT  DC    PL6'0'                                                           
WCOUNT   DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
*                                                                               
         DS    0D                                                               
RWCNT    DC    PL6'0'                                                           
ROCNT    DC    PL6'0'                                                           
TAPWCNT  DC    PL6'0'                                                           
TAPOCNT  DC    PL6'0'                                                           
TAPSCNT  DC    PL6'0'                                                           
REPCOUNT DC    PL6'0'                                                           
TPCH     DC    H'1878'                                                          
EXES     DC    120C'X'                                                          
NEGSW    DC    C'N'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATE OPTIONS                                       *         
***********************************************************************         
VOPTS    NTR1  BASE=*                                                           
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'0'                                                   
         MVI   CHECKOPT,C'N'                                                    
         MVI   CORPOPT,C'N'                                                     
         MVI   GPVOPT,C'N'                                                      
         MVI   RESUBOPT,C'0'                                                    
         MVI   QTROPT,0                                                         
         MVC   TLCN,MYSPACES                                                    
         XC    SSNSTART,SSNSTART                                                
         XC    CTRLNUM,CTRLNUM                                                  
*                                                                               
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         JZ    BADOPT                                                           
*                                                                               
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
*                                                                               
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         JZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
*                                                                               
OPT6     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT8                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         JZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
*                                                                               
OPT8     CLC   12(5,R4),=C'PRINT'  PRINT OPTION                                 
         BNE   OPT10                                                            
         MVC   PRINTOPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
OPT10    CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   OPT12                                                            
         MVC   TAPEOPT,22(R4)                                                   
         B     OPTEND                                                           
*                                                                               
OPT12    CLC   12(4,R4),=C'SUBS'   OPTION TO DO SUBS                            
         BNE   OPT14                                                            
         MVC   SUBSOPT,22(R4)                                                   
         B     OPTEND                                                           
*                                                                               
OPT14    CLC   12(8,R4),=C'CHECKADD'   OPTION JUST TO CHECK ADDRESSES           
         BNE   OPT16                                                            
         MVI   CHECKOPT,C'Y'                                                    
         B     OPTEND                                                           
*                                                                               
OPT16    CLC   12(4,R4),=C'DIAG'   DIAGNOSTIC OPTION                            
         BNE   OPT18                                                            
         MVC   DIAGOPT,22(R4)                                                   
         B     OPTEND                                                           
*                                                                               
OPT18    CLC   12(4,R4),=C'CORP'   CORP OPTION                                  
         BNE   OPT20                                                            
         MVI   CORPOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
OPT20    CLC   12(5,R4),=C'RESUB'  RESUB INDICATOR                              
         BNE   OPT22                                                            
         MVI   RESUBOPT,C'1'                                                    
         L     R1,8(R4)            GET TLCN NUMBER                              
         LTR   R1,R1                                                            
         JZ    INVXIT                                                           
         CVD   R1,DUB                                                           
         MVC   TLCN,DUB+6                                                       
         B     OPTEND                                                           
*                                                                               
OPT22    CLC   12(3,R4),=C'QTR'    QUARTER?                                     
         BNE   OPT24                                                            
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         JZ    BADOPT                                                           
         STC   R1,QTROPT                                                        
         B     OPTEND                                                           
*                                                                               
OPT24    CLC   12(3,R4),=C'SSN'    STARTING SSN                                 
         BNE   OPT26                                                            
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         JZ    INVXIT                                                           
         MVC   SSNSTART,22(R4)                                                  
         LA    RE,22(R4)                                                        
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'80',(RE))                                 
         MVC   TIFSSN,TGSSN                                                     
         MVC   TIQSTART(L'TIFSSN),TIFSSN                                        
         B     OPTEND                                                           
*                                                                               
OPT26    CLC   12(3,R4),=C'GPV'    GPV ONLY OPTION                              
         BNE   OPT28                                                            
         MVI   GPVOPT,C'Y'                                                      
         B     OPTEND                                                           
*                                                                               
OPT28    CLC   12(7,R4),=C'CONTROL'                                             
         BNE   OPT30                                                            
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         JZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   CTRLNUM,DUB                                                      
         B     OPTEND                                                           
*                                                                               
OPT30    DS    0H                                                               
         J     BADOPT                                                           
*                                                                               
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         J     XIT                                                              
*                                                                               
*              HEADLINES ETC                                                    
*                                                                               
HOOK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
*                                                                               
         MVC   H1+52(24),MYTITLE                                                
*                                                                               
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
*                                                                               
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
*                                                                               
         MVC   H3(9),=C'EMPLOYER:'                                              
         MVC   H3+10(2),SPLEMP                                                  
         MVC   H3+14(5),=C'UNIT:'                                               
         MVC   H3+20(3),DCBSTATE                                                
         MVC   H3+59(5),=C'YEAR:'                                               
         MVC   H3+65(4),SPLYEAR                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         LAY   R1,CKH6                                                          
         MVC   H6,0(R1)                                                         
         LAY   R1,CKH7                                                          
         MVI   H7+1,0                                                           
*                                                                               
         CLI   SUBSOPT,C'Y'                                                     
         BNE   *+10                                                             
         MVC   H7,0(R1)                                                         
*                                                                               
         CLI   CHECKOPT,C'Y'                                                    
         BE    HOOK2                                                            
*                                                                               
         LAY   R1,MYH6                                                          
         MVC   H6,0(R1)                                                         
         LAY   R1,MYH7                                                          
         MVC   H7,0(R1)                                                         
*                                                                               
         CLI   TAPETYPE,FEDERAL                                                 
         BE    HOOK2                                                            
*                                                                               
         CLI   TIFUNIT+2,C' '                                                   
         BH    HOOK1                                                            
         LAY   R1,MYH6STA                                                       
         MVC   H6,0(R1)                                                         
         LAY   R1,MYH7STA                                                       
         MVC   H7,0(R1)                                                         
*                                                                               
         CLC   TIFUNIT(2),=C'NJ'                                                
         BNE   HOOK2                                                            
*                                                                               
         LAY   R1,MYH6NJ                                                        
         MVC   H6,0(R1)                                                         
         LAY   R1,MYH7NJ                                                        
         MVC   H7,0(R1)                                                         
         B     HOOK2                                                            
*                                                                               
HOOK1    LAY   R1,MYH6CTY                                                       
         MVC   H6,0(R1)                                                         
         LAY   R1,MYH7CTY                                                       
         MVC   H7,0(R1)                                                         
*                                                                               
HOOK2    CLI   ACTEQU,ACTDOWN      DOWNLOAD?                                    
         BE    HOOK4                                                            
*                                                                               
         L     R6,ABOX                                                          
         USING BOXD,R6                                                          
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LAY   R1,MYCOLS                                                        
         MVC   BOXCOLS,0(R1)                                                    
*                                                                               
         CLI   CHECKOPT,C'Y'                                                    
         BNE   HOOK4                                                            
*                                                                               
         LAY   R1,CKCOLS                                                        
         MVC   BOXCOLS,0(R1)                                                    
*                                                                               
         DROP  R6                                                               
HOOK4    XIT1                                                                   
*                                                                               
APERH    DS    A                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              ROUTINE TO GET FICA WAGES                                        
*                                                                               
GETFICA  NTR1  BASE=*,LABEL=*                                                   
         L     R4,=A(LIMBLOCK)                                                  
         USING TMD,R4                                                           
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAYEELQ      LOOK FOR YTD ELEMENT                         
         BRAS  RE,GETEL            IN W2 RECORD                                 
         BNE   XIT                 AND, IF ITS THERE                            
         USING TAYED,R6                                                         
*                                                                               
         ST    RC,TMRC                                                          
         MVC   WORK(2),TIFYEAR                                                  
         MVC   WORK+2(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(0,TMEFDTE0) SET INTERNAL FORMAT            
         MVC   TMEMP,TIEMP                                                      
         MVC   TMUNIT,=C'FD'                                                    
         OI    TMSTAT,TMSCHK+TMSPAMT                                            
         MVC   TMTAXBLE(TMTNAMT*4),TAYENYTD                                     
*                                                                               
         GOTO1 =V(TALIM),DMCB,(R4)                                              
         J     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
STATECDE DC    C'AL ',C'01'      ALABAMA                                        
         DC    C'AK ',C'02'      ALASKA                                         
         DC    C'AZ ',C'04'      ARIZONA                                        
         DC    C'AR ',C'05'      ARKANSAS                                       
         DC    C'CA ',C'06'      CALIFORNIA                                     
         DC    C'CO ',C'08'      COLORADO                                       
         DC    C'CT ',C'09'      CONNECTICUT                                    
         DC    C'DE ',C'10'      DELAWARE                                       
         DC    C'DC ',C'11'      D.C.                                           
         DC    C'FL ',C'12'      FLORIDA                                        
         DC    C'GA ',C'13'      GEORGIA                                        
         DC    C'HI ',C'15'      HAWAII                                         
         DC    C'ID ',C'16'      IDAHO                                          
         DC    C'IL ',C'17'      ILLINOIS                                       
         DC    C'IN ',C'18'      INDIANA                                        
         DC    C'IA ',C'19'      IOWA                                           
         DC    C'KS ',C'20'      KANSAS                                         
         DC    C'KY ',C'21'      KENTUCKY                                       
         DC    C'LA ',C'22'      LOUISIANA                                      
         DC    C'ME ',C'23'      MAINE                                          
         DC    C'MD ',C'24'      MARYLAND                                       
         DC    C'MA ',C'25'      MASS.                                          
         DC    C'MI ',C'26'      MICHAGAN                                       
         DC    C'MN ',C'27'      MINNESOTA                                      
         DC    C'MS ',C'28'      MISSISSIPPI                                    
         DC    C'MO ',C'29'      MISSOURI                                       
         DC    C'MT ',C'30'      MONTANA                                        
         DC    C'NE ',C'31'      NEBRASKA                                       
         DC    C'NV ',C'32'      NEVADA                                         
         DC    C'NH ',C'33'      NEW HAMPSHIRE                                  
         DC    C'NJ ',C'34'      NEW JERSEY                                     
         DC    C'NM ',C'35'      NEW MEXICO                                     
         DC    C'NY ',C'36'      NEW YORK                                       
         DC    C'NC ',C'37'      NORTH CAROLINA                                 
         DC    C'ND ',C'38'      NORTH DAKOTA                                   
         DC    C'OH ',C'39'      OHIO                                           
         DC    C'OK ',C'40'      OKLAHOMA                                       
         DC    C'OR ',C'41'      OREGON                                         
         DC    C'PA ',C'42'      PENNSYLVANIA                                   
         DC    C'RI ',C'44'      RHODE ISLAND                                   
         DC    C'SC ',C'45'      SOUTH CAROLINA                                 
         DC    C'SD ',C'46'      SOUTH DAKOTA                                   
         DC    C'TN ',C'47'      TENNESSEE                                      
         DC    C'TX ',C'48'      TEXAS                                          
         DC    C'UT ',C'49'      UTAH                                           
         DC    C'VT ',C'50'      VERMONT                                        
         DC    C'VA ',C'51'      VIRGINIA                                       
         DC    C'WA ',C'53'      WASHINGTON                                     
         DC    C'WV ',C'54'      WEST VIRGINIA                                  
         DC    C'WI ',C'55'      WISCONSIN                                      
         DC    C'WY ',C'56'      WYOMING                                        
*                                                                               
         DC    C'PR ',C'PR'      PUERTO RICO                                    
         DC    C'PHL',C'42'      PHILADELPHIA                                   
         DC    X'FFFFFF'                                                        
         EJECT                                                                  
********                           NOT DIRECTLY ADDRESSABLE                     
*                                                                               
CHKACCS  DS    0D                                                               
OKACCS   DC    PL4'0',PL4'0',CL24'FAULTLESS RECORDS'                            
BADACCS  DC    PL4'0',PL4'0',CL24'RECORDS IN ERROR'                             
ERRACCS  DC    PL4'0',PL4'0',CL24'BAD/MISSING ZIP CODE'                         
         DC    PL4'0',PL4'0',CL24'BAD/MISSING STATE'                            
         DC    PL4'0',PL4'0',CL24'BAD/MISSING CITY'                             
         DC    PL4'0',PL4'0',CL24'STATE CHANGED/MATCH ZIP'                      
         DC    PL4'0',PL4'0',CL24'ADDRESS TOO LONG'                             
         DC    PL4'0',PL4'0',CL24' '                                            
         DC    PL4'0',PL4'0',CL24' '                                            
         DC    PL4'0',PL4'0',CL24' '                                            
*                                                                               
DIAGACCS DC    PL4'0',PL4'0',CL24'STATE WAS TRANSLATED'                         
         DC    PL4'0',PL4'0',CL24'STATE WAS DEDUCED'                            
         DC    PL4'0',PL4'0',CL24'LINE REMOVED TO FIT'                          
         DC    PL4'0',PL4'0',CL24'SPLIT STATE/ZIP'                              
         DC    PL4'0',PL4'0',CL24' '                                            
         DC    PL4'0',PL4'0',CL24' '                                            
         DC    PL4'0',PL4'0',CL24' '                                            
         DC    PL4'0',PL4'0',CL24' '                                            
         EJECT                                                                  
*              HEADINGS AND BOXES (NOT DIRECTLY ADDRESSABLE)                    
*                                                                               
MYH6     DS    0H                                                               
         DC    CL01' '                                                          
         DC    CL30'EMPLOYEE'                                                   
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12' FED WAGES'                                                 
         DC    CL01' '                                                          
         DC    CL12' FED TAXES'                                                 
         DC    CL01' '                                                          
         DC    CL12' FICA WAGES'                                                
         DC    CL01' '                                                          
         DC    CL12' FICA TAXES'                                                
         DC    CL01' '                                                          
         DC    CL12' MED. WAGES'                                                
         DC    CL01' '                                                          
         DC    CL12'  MEDICARE'                                                 
         DC    CL01' '                                                          
         DC    CL12' FRINGE BEN'                                                
         DC    CL01' '                                                          
*                                                                               
MYH7     DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12' STATE WAGES'                                               
         DC    CL01' '                                                          
         DC    CL12' STATE TAXES'                                               
         DC    CL01' '                                                          
         DC    CL12' LOCAL WAGES'                                               
         DC    CL01' '                                                          
         DC    CL12' LOCAL TAXES'                                               
         DC    CL01' '                                                          
         DC    CL12'  DISABILITY'                                               
         DC    CL01' '                                                          
         DC    CL12'      SUI   '                                               
         DC    CL01' '                                                          
         DC    CL12' '                                                          
         DC    CL01' '                                                          
*                                                                               
MYH6STA  DS    0H                                                               
         DC    CL01' '                                                          
         DC    CL30'EMPLOYEE'                                                   
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'   STATE '                                                  
         DC    CL01' '                                                          
         DC    CL12'   STATE '                                                  
         DC    CL01' '                                                          
         DC    CL12'    SDI   '                                                 
         DC    CL01' '                                                          
         DC    CL12'   LOCAL  '                                                 
         DC    CL01' '                                                          
         DC    CL12'   LOCAL  '                                                 
         DC    CL01' '                                                          
*                                                                               
MYH7STA  DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
*                                                                               
MYH6NJ   DS    0H                                                               
         DC    CL01' '                                                          
         DC    CL30'EMPLOYEE'                                                   
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'   STATE '                                                  
         DC    CL01' '                                                          
         DC    CL12'   STATE '                                                  
         DC    CL01' '                                                          
         DC    CL12'    SDI   '                                                 
         DC    CL01' '                                                          
         DC    CL12'    SUI   '                                                 
         DC    CL01' '                                                          
         DC    CL12'          '                                                 
         DC    CL01' '                                                          
*                                                                               
MYH7NJ   DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
*                                                                               
MYH6CTY  DS    0H                                                               
         DC    CL01' '                                                          
         DC    CL30'EMPLOYEE'                                                   
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'  FEDERAL '                                                 
         DC    CL01' '                                                          
         DC    CL12'   LOCAL '                                                  
         DC    CL01' '                                                          
         DC    CL12'   LOCAL '                                                  
         DC    CL01' '                                                          
         DC    CL12'          '                                                 
         DC    CL01' '                                                          
         DC    CL12'          '                                                 
         DC    CL01' '                                                          
         DC    CL12'          '                                                 
         DC    CL01' '                                                          
*                                                                               
MYH7CTY  DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   WAGES'                                                   
         DC    CL01' '                                                          
         DC    CL12'   TAXES'                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
         DC    CL12'        '                                                   
         DC    CL01' '                                                          
*                                                                               
CKH6     DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL33'EMPLOYEE NAME'                                              
         DC    CL01' '                                                          
         DC    CL30'CURRENT W4 ADDRESS'                                         
         DC    CL01' '                                                          
         DC    CL40'GENERATED STREET ADDRESS'                                   
         DC    CL01' '                                                          
         DC    CL24'DIAGNOSTICS'                                                
         DC    CL01' '                                                          
*                                                                               
CKH7     DS    0D                                                               
         DC    CL01' '                                                          
         DC    CL33'    AND SS#'                                                
         DC    CL01' '                                                          
         DC    CL30' '                                                          
         DC    CL01' '                                                          
         DC    CL40'CITY, STATE AND ZIP'                                        
         DC    CL01' '                                                          
         DC    CL24' '                                                          
         DC    CL01' '                                                          
*                                                                               
CKCOLS   DS    0D                                                               
         DC    CL01'L'                                                          
         DC    CL33' '                                                          
         DC    CL01'C'                                                          
         DC    CL30' '                                                          
         DC    CL01'C'                                                          
         DC    CL40' '                                                          
         DC    CL01'C'                                                          
         DC    CL24' '                                                          
         DC    CL01'R'                                                          
*                                                                               
MYCOLS   DS    0D                                                               
         DC    CL01'L'                                                          
         DC    CL39' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'C'                                                          
         DC    CL12' '                                                          
         DC    CL01'R'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*              FEDERAL W2 TAPE RECORD AREAS                                     
*                                                                               
WRA      DS    0CL275              RECORD -A- TRANSMITTER                       
WRAID    DC    CL1'A'              CODE A                                       
WRAYR    DC    CL4'    '           YEAR                                         
WRAEIN   DS    CL9                 FED ID NO                                    
         DC    CL8' '                                                           
         DC    CL1' '                                                           
WRATNM   DC    CL50' '             TRANS NAME                                   
WRATSTR  DC    CL40' '                   STREET                                 
WRATCITY DC    CL25' '                   CITY                                   
WRATST   DC    CL2' '                    STATE                                  
         DC    CL13' '                                                          
WRATZIP  DS    CL5                 ZIP CODE                                     
         DC    CL5' '                                                           
         DC    CL112' '                                                         
*                                                                               
WRANY    DS    0CL128              NEW YORK A RECORD                            
NYAID    DC    CL2'1A'             CODE 1A                                      
NYAYR    DC    CL4'    '           YEAR                                         
NYASID   DS    CL11                FED ID NO                                    
         DC    CL1' '                                                           
NYATNM   DC    CL40' '             TRANS NAME                                   
         DC    CL1' '                                                           
NYATSTR  DC    CL30' '                   STREET                                 
NYATCITY DC    CL25' '                   CITY                                   
NYATST   DC    CL2' '                    STATE                                  
NYATZIP  DC    CL9' '              ZIP CODE                                     
         DC    CL3' '                                                           
         EJECT                                                                  
*              B RECORD                                                         
*                                                                               
WRB      DS    0CL275              RECORD -B- BASIC AUTH                        
WRBID    DC    CL1'B'              CODE B                                       
WRBYR    DC    CL4'    '           YEAR                                         
WRBEIN   DS    CL9                 FED ID NO                                    
WRBIBM   DC    CL8'IBM'            COMPUTER MFR.                                
WRBILBL  DC    CL2'SL'             LABEL                                        
WRBPAR   DC    CL1' '                                                           
WRBDEN   DC    CL2'62'             6250 BPI DENSITY                             
WRBEBC   DC    CL3'EBC'            EBCDIC IND                                   
         DC    CL115' '                                                         
         DC    CL1' '                                                           
WRBTNM   DC    CL44' '             FILE RETURN NAME                             
WRBTSTR  DC    CL35' '             ADDRESS                                      
WRBTCITY DC    CL20' '             CITY                                         
WRBTST   DS    CL2                 STATE                                        
         DC    CL5' '                                                           
WRBTZIP  DS    CL5                 ZIP                                          
         DC    CL5' '                                                           
         DC    CL13' '                                                          
         EJECT                                                                  
*              E RECORD                                                         
*                                                                               
WRE      DS    0CL275              RECORD -E- EMPLR/ESTAB                       
WREID    DC    CL1'E'              CODE                                         
WREYR    DC    CL4'    '           YEAR                                         
WREEIN   DS    CL9                 FED ID NO                                    
         DC    CL9' '                                                           
WRETNM   DC    CL50' '             COMP NAME                                    
WRETSTR  DC    CL40' '             STREET                                       
WRETCITY DC    CL25' '             CITY                                         
WRETST   DS    CL2                 STATE                                        
         DC    CL8' '              REST OF THE STATE (N/U)                      
WREEFPZ  DC    CL5' '              FOREIGN POSTAL CODE (N/U)                    
WRETZIP  DS    CL5                 ZIP CODE                                     
WRENMCD  DC    CL1'F'              FIRST NAME FIRST                             
WRETPEM  DC    CL1'R'              REGULAR EMPL. TYPE                           
WRETPBLK DC    CL2'25'             BLOCKING FACTOR                              
WRETBLNK DC    CL5' '                                                           
         DC    CL1'A'              TAX TYPE CODE                                
WRESTCD1 DC    CL2'  '             STATE CODE 1                                 
         DC    CL5' '                                                           
WRESTID  DC    CL12' '             STATE ID                                     
         DC    CL1'A'              TAX TYPE CODE 2                              
WRESTCD2 DC    CL2'  '             STATE CODE 2                                 
         DC    CL88' '                                                          
*                                                                               
WRENY    DS    0CL128              NEW YORK E RECORD                            
NYEID    DC    CL2'1E'             CODE                                         
NYEYR    DC    CL4'    '           YEAR                                         
NYESID   DS    CL11                FED ID NO                                    
         DC    CL1' '                                                           
NYENM    DC    CL40' '             COMP NAME                                    
         DC    CL1' '                                                           
NYESTR   DC    CL30' '             STREET                                       
NYECITY  DC    CL25' '             CITY                                         
NYEST    DC    CL2'IL'             STATE                                        
NYEZIP   DC    CL9' '              ZIP CODE                                     
         DC    CL3' '                                                           
         EJECT                                                                  
*              O RECORD                                                         
*                                                                               
WRO      DS    0CL275                                                           
         EJECT                                                                  
*              W RECORD                                                         
*                                                                               
WRW      DS    0CL275              RECORD -W- EMPLOYEE WAGE                     
WRWID    DC    CL1'W'              CODE                                         
WRWSS    DS    CL9                 SS NO                                        
WRWNM    DS    CL27                NAME                                         
WRWAD    DC    CL40' '             STREET                                       
WRWCITY  DC    CL25' '             CITY                                         
WRWST    DC    CL10' '             STATE                                        
WRWFPC   DC    CL5' '                                                           
WRWZIP   DS    CL5                 ZIP CODE                                     
         DC    C' '                                                             
WRWFICAW DS    CL7                 FICA WAGES                                   
         DC    CL1' '                                                           
         DC    CL7'0000000'        FICA TIPS (N/U)                              
         DC    CL1' '                                                           
WRWWAGES DS    CL9                 TOTAL WAGES                                  
         DC    CL1' '                                                           
WRWFICAT DS    CL6                 FICA TAX                                     
WRWFWT   DS    CL9                 FED WITH. TAX                                
         DC    CL1' '                                                           
         DC    CL7'0000000'        ALLOCATED TIPS (N/U)                         
         DC    CL1' '                                                           
WRWFB    DC    CL9'000000000'      FRINGE BENEFITS (N/U)                        
WRWMEDW  DC    CL9'000000000'      MEDICARE WAGES (N/U)                         
WRWMWT   DC    CL7'0000000'        MEDICARE TAXES (N/U)                         
         DC    CL8' '                                                           
         DC    CL9'000000000'      SECTION 457 (N/U)                            
         DC    CL1' '                                                           
         DC    CL9'000000000'      NOT SECTION 457 (N/U)                        
         DC    CL1' '                                                           
         DC    CL7'0000000'        DEPENDANT CARE BENEFITS                      
WRWCTL   DS    CL7                 CONTROL SEQ. NO.                             
         DC    CL7'0000000'                                                     
         DC    CL7'0000000'                                                     
         DC    CL7'0000000'                                                     
         DC    CL1' '                                                           
WRWPEN   DC    CL1' '              PENSION PLAN INDIC                           
         DC    CL1' '                                                           
         DC    CL1' '                                                           
         DC    CL1' '                                                           
         DC    CL9'000000000'      DEFERRED COMP. (N/U)                         
         EJECT                                                                  
*              W RECORD FOR ILLINOIS                                            
*                                                                               
WRWIL    DS    0CL275              W RECORD FOR ILLINOIS                        
ILWID    DC    CL1'W'              CODE                                         
ILWSS    DS    CL9                 SS NO                                        
ILWNM    DS    CL27                NAME                                         
ILWAD    DC    CL40' '             STREET                                       
ILWCITY  DC    CL25' '             CITY                                         
ILWST    DC    CL10' '             STATE                                        
ILWFPC   DC    CL5' '                                                           
ILWZIP   DS    CL5                 ZIP CODE                                     
         DC    C' '                                                             
ILWFICAW DS    CL7                 FICA WAGES                                   
         DC    CL1' '                                                           
         DC    CL7'0000000'        FICA TIPS (N/U)                              
         DC    CL1' '                                                           
ILWWAGES DS    CL9                 TOTAL WAGES                                  
         DC    CL1' '                                                           
ILWFICAT DS    CL6                 FICA TAX                                     
ILWFWT   DS    CL9                 FED WITH. TAX                                
         DC    CL1' '                                                           
         DC    CL7'0000000'        ALLOCATED TIPS (N/U)                         
         DC    CL1' '                                                           
ILWFB    DC    CL9'000000000'      FRINGE BENEFITS (N/U)                        
ILWSTCD  DS    CL2                 STATE CODE (IL=17)                           
         DC    CL1' '                                                           
ILWSTWAG DC    CL9'000000000'      STATE WAGES                                  
ILWSTTAX DC    CL8'00000000'       STATE TAXES                                  
ILWVLSDI DC    CL5'00000'          VOL. SDI                                     
ILWSDI   DC    CL5'00000'          SDI                                          
         DC    CL1' '                                                           
         DC    CL5'00000'          ENTITY CODE                                  
ILWLOWAG DC    CL9'000000000'      LOCAL WAGES                                  
ILWLOTAX DC    CL7'00000000'       LOCAL TAXES                                  
         DC    CL7' '                                                           
         DC    CL7'00000000'                                                    
         DC    CL7'00000000'                                                    
         DC    CL7'00000000'                                                    
         DC    CL1' '                                                           
ILWPEN   DC    CL1' '                                                           
         DC    CL3' '                                                           
         DC    CL9'000000000'      DEFERRED COMP. (N/U)                         
         EJECT                                                                  
*              W RECORDS FOR NEW YORK                                           
*                                                                               
WRWNY    DS    0CL128              1W RECORD FOR NY                             
NYWID    DC    CL2'1W'             CODE                                         
NYWSS    DS    CL9                 SS NO                                        
NYWNM    DS    CL30                NAME                                         
NYWAD    DC    CL40' '             STREET                                       
NYWCITY  DC    CL25' '             CITY                                         
NYWST    DC    CL2' '              STATE                                        
NYWZIP   DS    CL5                 ZIP CODE                                     
         DC    CL15' '                                                          
*                                                                               
WRWNY2   DS    0CL128              2W RECORD FOR NY                             
         DC    CL2'2W'             CODE                                         
NYWSS2   DS    CL9                 SS NO                                        
         DC    CL1' '                                                           
NYWSWT   DC    CL9'000000000'      NY STATE TAX                                 
         DC    CL1' '                                                           
NYWNWT   DC    CL9'000000000'      NYC TAX                                      
         DC    CL1' '                                                           
NYWYWT   DC    CL9'000000000'      YONKERS TAX                                  
         DC    CL1' '                                                           
NYWWAGES DC    CL11'00000000000'   WAGES                                        
         DC    CL1' '                                                           
         DC    CL9'000000000'      TIPS                                         
         DC    CL1' '                                                           
         DC    CL11'00000000000'   BENEFITS                                     
         DC    CL1' '                                                           
         DC    CL11'00000000000'   ADDBACK                                      
         DC    CL43' '                                                          
         EJECT                                                                  
*              S RECORD                                                         
*                                                                               
WRS      DS    0CL275              RECORD -S- SUBSIDIARY RECORD                 
WRSCD    DC    CL1'S'              CODE                                         
WRSSS    DS    CL9                 SS NO                                        
WRSNM    DS    CL27                NAME                                         
WRSAD    DC    CL40' '             STREET                                       
WRSCITY  DC    CL25' '             CITY                                         
WRSST    DC    CL10' '             STATE                                        
WRSFPC   DC    CL5' '                                                           
WRSZIP   DS    CL5                 ZIP CODE                                     
         DC    C' '                                                             
WRSSTA   DS    CL2                 STATE CODE (NUMERIC!)                        
         DC    CL2' '                                                           
         DC    CL2'12'             MONTH                                        
WRSYEAR  DC    CL2'99'             YEAR                                         
         DC    CL9'000000000'                                                   
         DC    CL9'000000000'                                                   
         DC    CL2' '                                                           
         DC    CL4' '                                                           
         DC    CL4' '                                                           
         DC    CL5' '                                                           
WRSID    DC    CL12' '                                                          
         DC    CL6' '                                                           
WRSSTA2  DS    CL2                 STATE CODE AGAIN!                            
WRSWAGES DC    CL9'000000000'      STATE WAGES                                  
WRSTAXES DC    CL8'00000000'       STATE TAXES                                  
WRSSUI   DC    CL5'00000'          SUI                                          
WRSSDI   DC    CL5'00000'          DISABILITY                                   
         DC    C'C'                CITY INCOME TAX INDIC                        
WRSLOCNT DC    CL5' '                                                           
WRSWAGEL DC    CL9'000000000'      LOCAL WAGES                                  
WRSTAXL  DC    CL7'0000000'        LOCAL TAXES                                  
         DC    CL7' '                                                           
WRSAD2   DC    CL35' '             2ND ADDRESS LINE FOR UARCO                   
         EJECT                                                                  
*              T RECORD                                                         
*                                                                               
WRT      DS    0CL275              RECORD -T- TOTAL                             
WRTID    DC    CL1'T'              CODE T                                       
WRTNEMPS DS    CL7                 TOTAL NO. OF -W- RECORDS                     
WRTFICAW DS    CL13                TOTAL FICA WAGES                             
         DC    CL1' '                                                           
         DC    CL12'000000000000'  FICA TIPS,N/U                                
WRTWAGES DS    CL13                TOTAL WAGES                                  
         DC    CL1' '                                                           
WRTFICAT DS    CL12                FICA TAX                                     
         DC    CL1' '                                                           
WRTFWT   DS    CL12                FWT                                          
         DC    CL12'000000000000'  LIFE INS,N/U                                 
         DC    CL12'000000000000'  UNCOLL. TAX ON TIPS, N/U                     
         DC    CL12'000000000000'  ADV. EIC,N/U                                 
         DC    CL12'000000000000'  ALLOCATED TIPS                               
WRTFB    DC    CL12'000000000000'  FRINGE BENEFITS                              
         DC    CL1' '                                                           
         DC    CL13'0000000000000'                                              
         DC    CL1' '                                                           
         DC    CL12'0000000000000'                                              
         DC    CL1' '                                                           
         DC    CL13'0000000000000'                                              
         DC    CL1' '                                                           
         DC    CL13'0000000000000'                                              
         DC    CL1' '                                                           
WRTMEDW  DC    CL13'0000000000000'  MEDICARE WAGES                              
         DC    CL1' '                                                           
WRTMEDT  DC    CL12'0000000000000'  MEDICARE TAXES                              
         DC    CL60' '                                                          
         EJECT                                                                  
*              T RECORD FOR ILLINOIS                                            
*                                                                               
WRTIL    DS    0CL275              T RECORD FOR ILLINOIS                        
ILTID    DC    CL1'T'              CODE T                                       
ILTNEMPS DS    CL7                 TOTAL NO. OF -W- RECORDS                     
         DC    CL13'0000000000000' TOTAL FICA WAGES                             
         DC    CL1' '                                                           
         DC    CL12'000000000000'  FICA TIPS,N/U                                
         DC    CL13'0000000000000' TOTAL WAGES                                  
         DC    CL1' '                                                           
         DC    CL12'000000000000'  FICA TAX                                     
         DC    CL1' '                                                           
         DC    CL12'000000000000'  FWT                                          
         DC    CL12'000000000000'  LIFE INS,N/U                                 
         DC    CL12'000000000000'  UNCOLL. TAX ON TIPS, N/U                     
         DC    CL12'000000000000'  ADV. EIC,N/U                                 
         DC    CL12'000000000000'  ALLOCATED TIPS                               
         DC    CL12'000000000000'  FRINGE BENEFITS                              
         DC    CL1' '                                                           
         DC    CL13'0000000000000'                                              
         DC    CL58' '                                                          
ILTSTCD  DC    CL2'17'             STATE CODE                                   
         DC    CL1' '                                                           
         DC    CL7'0000000'        EMP CNT                                      
         DC    CL1' '                                                           
ILTSTWAG DC    CL12'0000000000000' STATE WAGES                                  
         DC    CL1' '                                                           
ILTSTTAX DC    CL12'0000000000000' STATE TAXES                                  
         DC    CL1' '                                                           
         DC    CL12'0000000000000'                                              
         DC    CL1' '                                                           
ILTSDI   DC    CL12'0000000000000' STATE SDI                                    
         DC    CL8' '                                                           
         EJECT                                                                  
*              T RECORD FOR CALIFORNIA, GENERIC AND RHODE ISLAND                
*                                                                               
WRTCA    DS    0CL275              T RECORD FOR CALIFORNIA ETC.                 
CATID    DC    CL1'T'              CODE T                                       
CATNEMPS DS    CL7                 TOTAL NO. OF -W- RECORDS                     
CATWGSRI DC    CL13'0000000000000' TOTAL FICA WAGES (STATE WAGES -RI)           
         DC    CL1' '                                                           
CATTAXRI DC    CL12'000000000000'  FICA TIPS (STATE TAXES - RI)                 
CATWAGES DC    CL13'0000000000000' TOTAL WAGES                                  
         DC    CL1' '                                                           
         DC    CL12'000000000000'  FICA TAX                                     
         DC    CL1' '                                                           
CATFWT   DC    CL12'000000000000'  FWT                                          
         DC    CL12'000000000000'  LIFE INS,N/U                                 
         DC    CL12'000000000000'  UNCOLL. TAX ON TIPS, N/U                     
         DC    CL12'000000000000'  ADV. EIC,N/U                                 
         DC    CL12'000000000000'  ALLOCATED TIPS                               
         DC    CL12'000000000000'  FRINGE BENEFITS                              
         DC    CL1' '                                                           
         DC    CL13'0000000000000'                                              
         DC    CL58' '                                                          
CATSTCD  DC    CL2'06'             STATE CODE                                   
         DC    CL1' '                                                           
CATEMP2  DC    CL7'0000000'        EMP CNT                                      
         DC    CL1' '                                                           
CATSTWAG DC    CL12'0000000000000' STATE WAGES                                  
         DC    CL1' '                                                           
CATSTTAX DC    CL12'0000000000000' STATE TAXES                                  
         DC    CL1' '                                                           
         DC    CL12'0000000000000'                                              
         DC    CL1' '                                                           
CATSDI   DC    CL12'0000000000000' STATE SDI                                    
         DC    CL8' '                                                           
         EJECT                                                                  
*              T RECORD FOR NEW YORK                                            
*                                                                               
WRTNY    DS    0CL128              T RECORD FOR NY                              
NYTID    DC    CL2'1T'             CODE 1T                                      
NYTNEMPS DS    CL7                 TOTAL NO. OF -W- RECORDS                     
         DC    CL1' '                                                           
NYTSWT   DC    CL15'000000000000000'     STATE TAX                              
         DC    CL1' '                                                           
NYTNWT   DC    CL15'000000000000000'     NYC TAX                                
         DC    CL1' '                                                           
NYTYWT   DC    CL15'000000000000000'     YONKERS TAX                            
         DC    CL1' '                                                           
NYTWAGES DC    CL15'000000000000000'     STATE EARNINGS                         
         DC    CL55' '                                                          
*                                                                               
*              T RECORD FOR CONNECTICUT                                         
*                                                                               
WRTCT    DS    0CL275              T RECORD FOR CONNECTICUT                     
CTTID    DC    CL1'T'              CODE T                                       
CTTNEMPS DS    CL7                 TOTAL NO. OF -W- RECORDS                     
CTTWAGES DC    CL13'0000000000000' TOTAL STATE WAGES                            
         DC    CL1' '                                                           
CTTTAXES DC    CL12'000000000000'  TOTAL STATE TAX WITHHELD                     
         DC    CL241' '                                                         
         EJECT                                                                  
*              F RECORD                                                         
*                                                                               
WRF      DS    0CL275              RECORD -F- FINAL                             
WRFID    DC    CL1'F'              CODE F                                       
WRFNEMPS DS    CL7                 TOTAL NO. OF CODE W RCDS                     
         DC    CL1' '                                                           
WRFFICAW DS    CL16                TOTAL FICA WAGES (STATE WAGES -RI)           
         DC    CL1' '                                                           
WRFTAXRI DC    CL16'0000000000000000' FICA TIPS (STATE TAXES -RI)               
         DC    CL1' '                                                           
WRFWAGES DS    CL16                TOTAL WAGES                                  
         DC    CL1' '                                                           
WRFFICAT DS    CL16                FICA TAX                                     
         DC    CL1' '                                                           
WRFFWT   DS    CL16                FED TAX                                      
         DC    CL1' '                                                           
         DC    CL16'0000000000000000' ADV/EIC (N/U)                             
         DC    165C' '                                                          
*                                                                               
WRFIL    DS    0CL275              F RECORD FOR ILLINOIS                        
ILFID    DC    CL1'F'              CODE F                                       
ILFNEMPS DS    CL7                 TOTAL NO. OF CODE W RCDS                     
         DC    CL2'17'             STATE CODE                                   
ILFSTWAG DC    CL13'0000000000000' TOTAL STATE WAGES                            
ILFSTTAX DC    CL12'000000000000'  TOTAL STATE TAXES                            
         DC    CL240' '                                                         
*                                                                               
WRFNY    DS    0CL128              F RECORD FOR NY                              
NYFID    DC    CL2'1F'             CODE 1F                                      
NYFNEMPS DS    CL7                 TOTAL NO. OF CODE W RCDS                     
         DC    CL119' '                                                         
*                                                                               
WRFCT    DS    0CL275              F RECORD FOR CT                              
CTFID    DC    CL1'F'              CODE F                                       
CTFNEMPS DS    CL7                 TOTAL NO. OF CODE W RCDS                     
         DC    CL1' '                                                           
CTFWAGES DS    CL16'0000000000000000' TOTAL WAGES                               
         DC    CL1' '                                                           
CTFTAXES DC    CL16'0000000000000000' TOTAL TAXWITHELD                          
         DC    CL233' '                                                         
         EJECT                                                                  
*                                                                               
*              BLOCK FOR TALIM                                                  
*                                                                               
         DS    0D                                                               
LIMBLOCK DS    0CL(TMLNQ)                                                       
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
***********************************************************************         
*              2001 FEDERAL TAPE RECORD LAYOUTS                       *         
*              ALL RECORDS LISTED ARE REQUIRED                        *         
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
*              SUBMITTER RECORD - TYPE RA                             *         
*---------------------------------------------------------------------*         
*                                                                               
FRA      DS    0CL512                                                           
FRAID    DC    CL2'RA'             RECORD IDENTIFIER 'RA'                       
FRASEIN  DS    CL9                 SUBMITTER'S EIN                              
FRAPIN   DC    CL17' '             SUBMITTER'S PIN                              
FRARES   DS    CL1                 RESUB INDICATOR                              
FRATLCN  DS    CL6                 TLCN IF RESUBMISSION                         
FRASOFT  DC    CL2'98'             SOFTWARE CODE                                
*                                                                               
FRACNAM  DC    CL57' '             COMPANY NAME                                 
FRACLADD DS    CL22' '             LOCATION ADDRESS                             
FRACDADD DS    CL22' '             DELIVERY ADDRESS                             
FRACCITY DS    CL22' '             CITY                                         
FRACSTAT DS    CL2                 STATE                                        
FRACZIP  DS    CL5                 ZIP CODE                                     
FRACZIPX DS    CL4                 ZIP CODE EXTENSION                           
*                                                                               
         DC    CL5' '              RESERVED FOR SSA USE                         
FRAFSP   DC    CL23' '             FOREIGN STATE/PROVINCE                       
FRAFPC   DC    CL15' '             FOREIGN POSTAL CODE                          
FRACTRY  DC    CL2' '              COUNTRY CODE                                 
*                                                                               
FRASNAM  DC    CL57' '             SUBMITTER NAME                               
FRASLADD DS    CL22' '             LOCATION ADDRESS FOR SUBMITTER               
FRASDADD DS    CL22' '             DELIVERY ADDRESS FOR SUBMITTER               
FRASCITY DS    CL22' '             SUBMITTER CITY                               
FRASSTAT DS    CL2                 SUBMITTER STATE                              
FRASZIP  DS    CL5                 SUBMITTER ZIP CODE                           
FRASZIPX DS    CL4                 SUBMITTER ZIP CODE EXTENSION                 
*                                                                               
         DC    CL5' '              RESERVED FOR SSA USE                         
FRASFSP  DC    CL23' '             SUBMITTER FOREIGN STATE/PROVINCE             
FRASFPC  DC    CL15' '             SUBMITTER FOREIGN POSTAL CODE                
FRASCTRY DC    CL2' '              SUBMITTER COUNTRY CODE                       
*                                                                               
FRACTNAM DC    CL27'Julie Hegelmann' CONTACT NAME                               
FRACTPH  DC    CL15'3129237900'    CONTACT PHONE NUMBER                         
FRACTPHX DC    CL5'7905'           CONTACT PHONE NUMBER EXTENSION               
         DC    CL3' '              RESERVED FOR SSA                             
FRACINT  DS    0CL40               CONTACT EMAIL/INTERNET                       
         DC    CL40'JHegelmann@talentpartners.com'                              
         DC    CL3' '              RESERVED FOR SSA                             
FRACFAX  DC    CL10'3129439875'    CONTACT FAX                                  
*                                                                               
FRANCOD  DC    CL1' '              PROBLEM NOTIFICATION CODE                    
FRAPCOD  DC    CL1'L'              PREPARER CODE                                
         DC    CL12' '             RESERVED FOR SSA                             
FRALNQ   EQU   *-FRA                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              EMPLOYER RECORD - TYPE RE                              *         
*---------------------------------------------------------------------*         
*                                                                               
FRE      DS    0CL512                                                           
FREID    DC    CL2'RE'             RECORD IDENTIFER  'RE'                       
FREYR    DS    CL4                 TAX YEAR                                     
FREAIC   DC    CL1' '              AGENT INDICATOR CODE                         
FREEEIN  DS    CL9                 EMPLOYER/AGENT'S EIN                         
FREAEIN  DC    CL9' '              AGENT FOR EIN                                
FRETBI   DC    CL1'0'              TERMINATING BUSINESS INDICATOR               
FREEST   DC    CL4' '              ESTABLISHMENT NUMBER                         
FREOEIN  DC    CL9' '              OTHER EIN                                    
*                                                                               
FREENAM  DC    CL57' '             EMPLOYER NAME                                
FRELADD  DS    CL22' '             LOCATION ADDRESS                             
FREDADD  DS    CL22' '             DELIVERY ADDRESS                             
FRECITY  DS    CL22' '             CITY                                         
FRESTAT  DS    CL2                 STATE ABBREVIATION                           
FREZIP   DS    CL5                 ZIP CODE                                     
FREZIPX  DS    CL4                 ZIP CODE EXTENSION                           
*                                                                               
FREKIND  DC    CL1'N'              KIND OF EMPLOYER                             
         DC    CL4' '              RESERVED FOR SSA USE                         
*                                                                               
FREFSP   DC    CL23' '             FOREIGN STATE/PROVINCE                       
FREFPC   DC    CL15' '             FOREIGN POSTAL CODE                          
FRECTRY  DC    CL2' '              COUNTRY CODE                                 
*                                                                               
FREECOD  DC    CL1'R'              EMPLOYMENT CODE                              
FRETJUR  DC    CL1' '              TAX JURISDICTION CODE                        
FRETPSP  DC    CL1'0'              THIRD PARTY SICK PAY                         
*                                                                               
FRECTNAM DC    CL27'Julie Hegelmann' CONTACT NAME                               
FRECTPH  DC    CL15'3129237900'    CONTACT PHONE NUMBER                         
FRECTPHX DC    CL5'7905'           CONTACT PHONE NUMBER EXTENSION               
FRECFAX  DC    CL10'3129439875'    CONTACT FAX                                  
FRECINT  DS    0CL40               CONTACT EMAIL/INTERNET                       
         DC    CL40'JHegelmann@talentpartners.com'                              
         DC    194C' '             RESERVED FOR SSA USE                         
*                                                                               
         ORG   FRECTNAM                                                         
FREMDNUM DS    CL8' '              MD 8 DIGIT ACCOUNT NUMBER                    
         DS    171C' '             RESERVED FOR SSA USE                         
FREALNUM DS    CL10' '             AL 10 DIGIT ACCOUNT NUMBER                   
         DS    86C' '              RESERVED FOR SSA USE                         
*                                                                               
FREDATE  DS    8C' '               DATE - YYYYMMDD (MD ONLY)                    
FRETIME  DS    8C' '               TIMESTAMP - HHMMSSNN (MD ONLY)               
*                                                                               
         ORG   FRETIME                                                          
FREPANUM DS    CL8' '              PA 8 DIGIT ACCOUNT NUMBER                    
FRELNQ   EQU   *-FRE                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              EMPLOYER RECORD - TYPE RO                              *         
*---------------------------------------------------------------------*         
*                                                                               
SRTRO    DS    0C                  FOR SORTING                                  
SRTROST  DS    CL2                 STATE                                        
SRTROSS  DS    CL9                 SS#                                          
SRTRORC  DS    XL1                 X'02'                                        
SRTROLNQ EQU   *-SRTRO                                                          
*                                                                               
FRO      DS    0CL512                                                           
FROID    DC    CL2'RO'             RECORD IDENTIFER  'RO'                       
         DC    CL9' '              RESERVED FOR SSA USE                         
FROATIPS DC    11C'0'              ALLOCATED TIPS                               
FROUETXT DC    11C'0'              UNCOLLECTED EMPLOYEE TAX ON TIPS             
FROMSACC DC    11C'0'              MEDICAL SAVINGS ACCOUNT                      
FROSRACC DC    11C'0'              SIMPLE RETIREMENT ACCOUNT                    
FROQAEXP DC    11C'0'              QUALIFIED ADOPTION EXPENSES                  
*                                                                               
FROUCSST DC    11C'0'              UNCOLLECTED SS OR RRTA TAX > $50,000         
FROUCMED DC    11C'0'              UNCOLLECTED MEDICARE TAX > $50,000           
FROUS409 DC    11C'0'              INCOME UNDER SECTION 409A                    
FRONHAWG DC    CL11'0'             NEW HIRE ACT EXEMPT WAGES                    
**NOP    DC    154C' '             RESERVED FOR SSA USE                         
         DC    11C'0'                                                           
         DC    CL143' '            RESERVED FOR SSA USE                         
*                                                                               
FROCSTAT DS    CL1' '              CIVIL STATUS                                 
FROSPSSN DC    CL9' '              RESERVED FOR SSA USE                         
FROWGPRT DC    CL11'0'             WAGES SUBJECT TO PUERTO RICO TAX             
FROCMPRT DC    11C'0'              COMMISSIONS SUBJECT TO PR TAX                
FROALPRT DC    11C'0'              ALLOWANCES SUBJECT TO PR TAX                 
FROTPPRT DC    11C'0'              TIPS SUBJECT TO PUERTO RICO TAX              
FROTWPRT DS    CL11'0'             TOTAL WAGES, COMM, TIPS, AND ALLOW           
*                                  SUBJECT TO PUERTO RICO TAX                   
FROPRTWT DS    CL11'0'             PUERTO RICO TAX WITHHELD                     
FRORFAC  DC    11C'0'              RETIREMENT FUND ANNUAL CONTRIBUTIONS         
         DC    11C' '              RESERVED FOR SSA USE                         
*                                                                               
FROTWVI  DC    11C'0'              TOTAL WAGES, TIPS AND OTHER TO VI            
FROVIWT  DC    11C'0'              VIRGIN ISLAND TAX WITHHELD                   
         DC    128C' '             RESERVED FOR SSA USE                         
FROLNQ   EQU   *-FRO                                                            
FROLNQ2  EQU   *-FRORFAC           2ND LENGTH TO ZERO REST OF RECORD            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              EMPLOYEE WAGE RECORD - TYPE RW                         *         
*---------------------------------------------------------------------*         
*                                                                               
SRTRW    DS    0C                  FOR SORTING                                  
SRTRWST  DS    CL2                 STATE                                        
SRTRWSS  DS    CL9                 SS#                                          
SRTRWRC  DS    XL1                 X'01'                                        
SRTRWLNQ EQU   *-SRTRW                                                          
*                                                                               
FRW      DS    0CL512                                                           
FRWID    DC    CL2'RW'             RECORD IDENTIFIER 'RW'                       
FRWSS    DS    CL9                 SOCIAL SECURITY NUMBER                       
*                                                                               
FRWEFNAM DS    CL15                EMPLOYEE FIRST NAME                          
FRWEMIDN DS    CL15' '             EMPLOYEE MIDDLE NAME (INITIAL)               
FRWELNAM DS    CL16                EMPLOYEE LAST NAME                           
         DC    CL4' '                                                           
FRWESUFF DS    CL4' '              SUFFIX                                       
*                                                                               
FRWLADD  DS    CL22' '             LOCATION ADDRESS                             
FRWDADD  DS    CL22' '             DELIVERY ADDRESS                             
FRWCITY  DS    CL22' '             CITY                                         
FRWSTAT  DS    CL2                 STATE ABBREVIATION                           
FRWZIP   DS    CL5                 ZIP CODE                                     
FRWZIPX  DS    CL4                 ZIP CODE EXTENSION                           
*                                                                               
         DC    CL5' '              RESERVED FOR SSA USE                         
FRWFSP   DC    CL23' '             FOREIGN STATE/PROVINCE                       
FRWFPC   DC    CL15' '             FOREIGN POTAL CODE                           
FRWCTRY  DC    CL2' '              COUNTRY CODE                                 
*                                                                               
FRWWAGES DS    CL11                WAGES, TIPS AND OTHER COMPENSATION           
FRWFTAX  DS    CL11                FEDERAL TAX WITHHELD                         
FRWSSW   DS    CL11                SOCIAL SECURITY WAGES                        
FRWSSTAX DS    CL11                SOCIAL SECURITY TAX WITHHELD                 
FRWMCW   DS    CL11                MEDICARE WAGES                               
FRWMTAX  DS    CL11                MEDICARE TAX WITHHELD                        
*                                                                               
FRWSSTIP DC    11C'0'              SOCIAL SECURITY TIPS                         
         DC    11C' '              ADVANCE EARNED INCOME CREDIT                 
FRWDCB   DC    11C'0'              DEPENDENT CARE BENEFITS                      
FRWDC401 DC    11C'0'              DEFERRED COMPENSATION TO 401(K)              
FRWDC403 DC    11C'0'              DEFERRED COMPENSATION TO 403(B)              
FRWDC408 DC    11C'0'              DEFERRED COMPENSATION TO 408(K)(6)           
FRWDC457 DC    11C'0'              DEFERRED COMPENSATION TO 457(B)              
FRWDC501 DC    11C'0'              DEFERRED COMPENSATION TO 501(C) &            
*                                  DEFERRED COMPENSATION TO 18(D)               
FRWBQSC  DC    11C' '              BASIS QUARTERS, SUBSISTENCE & COMBAT         
FRWNQ457 DC    11C'0'              NON-QUALIFIED PLAN SECTION 457               
FRWEMPHS DC    11C'0'              EMPLOYER CONTRIB TO HEALTH SAVINGS           
FRWNQOTH DC    11C'0'              NON-QUALIFIED PLAN NOT SECTION 457           
FRWNCPAY DC    11C'0'              NON-TAXABLE COMBAT PAY                       
         DC    CL11' '             RESERVED FOR SSA USE                         
*                                                                               
FRWELIFE DC    11C'0'              EMPLOYEE COST FOR LIFE > $50,000             
FRWXSOPT DC    11C'0'              INCOME FROM EXERCISE OF STOCK OPTION         
FRWD409A DC    11C'0'              DEFERRALS UNDER SECTION 409A                 
FRW401K  DC    11C'0'              ROTH CONTRIBUTIONS TO 401K                   
FRW403B  DC    11C'0'              ROTH CONTRIBUTIONS TO 403B                   
**NOP    DC    CL23' '             RESERVED FOR SSA USE                         
         DC    11C'0'                                                           
         DC    CL12' '             RESERVED FOR SSA USE                         
*                                                                               
FRWWEI   DC    CL1'0'              STATUTORY EMPLOYEE INDICATOR                 
         DC    CL1' '              RESERVED FOR SSA USE                         
FRWRETPL DC    CL1'0'              RETIREMENT PLAN                              
FRWSICK  DC    CL1'0'              SICK PAY INDICATOR                           
         DC    CL23' '             RESERVED FOR SSA USE                         
FRWLNQ   EQU   *-FRW                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              STATE SUPP. EMPLOYEE RECORD - TYPE RS                            
*---------------------------------------------------------------------*         
*                                                                               
FRS      DS    0CL512                                                           
FRSID    DC    CL2'RS'             RECORD IDENTIFIER 'RS'                       
FRSSCODE DS    CL2' '              STATE CODE                                   
FRSTXENT DS    CL5' '              TAXING ENTITY CODE                           
FRSSS    DS    CL9                 SOCIAL SECURITY NUMBER                       
*                                                                               
FRSEFNAM DS    CL15                EMPLOYEE FIRST NAME                          
FRSEMIDN DS    CL15' '             EMPLOYEE MIDDLE NAME (INITIAL)               
FRSELNAM DS    CL16                EMPLOYEE LAST NAME                           
         DC    CL4' '                                                           
FRSESUFF DS    CL4' '              SUFFIX                                       
FRSNAMLN EQU   *-FRSEFNAM                                                       
*                                                                               
FRSLADD  DS    CL22' '             LOCATION ADDRESS                             
FRSDADD  DS    CL22' '             DELIVERY ADDRESS                             
FRSCITY  DS    CL22' '             CITY                                         
FRSSTAT  DS    CL2                 STATE ABBREVIATION                           
FRSZIP   DS    CL5                 ZIP CODE                                     
FRSZIPX  DS    CL4                 ZIP CODE EXTENSION                           
FRSADDLN EQU   *-FRSLADD                                                        
*                                                                               
FRSSSA   DC    CL5' '              RESERVED FOR SSA USE                         
FRSFSP   DC    CL23' '             FOREIGN STATE/PROVINCE                       
FRSFPC   DC    CL15' '             FOREIGN POTAL CODE                           
FRSCTRY  DC    CL2' '              COUNTRY CODE                                 
*                                  UNEMPLOYMENT REPORTING                       
FRSOPTCD DS    CL2                 OPTIONAL CODE                                
FRSRPTPD DS    CL6                 REPORTING PERIOD                             
FRSQTOTW DS    CL11                QTRLY UNEMPLYMNT INS TOTAL WAGES             
FRSQTTXW DS    CL11                QTRLY UNEMPLYMNT INS TOTAL TAX WAGES         
FRSWKWRK DS    CL2                 NUMBER OF WEEKS WORKED                       
FRSDTEMP DS    CL8                 DATE FIRST EMPLOYED                          
FRSDTSEP DS    CL8                 DATE OF SEPARATION                           
         DC    5C' '               BLANK                                        
FRSEMPAC DS    CL20                STATE EMPLOYER ACCOUNT NUMBER                
FRSUEMLN EQU   *-FRSOPTCD                                                       
*                                                                               
         ORG   FRSCTRY                                                          
FRSBLNK  DS    CL81                BLANK (INDIANA)                              
FRSSTCD2 DS    CL2                 STATE CODE                                   
FRSSWAG  DS    CL11                STATE TAXABLE WAGES                          
FRSSTAX2 DS    CL11                STATE TAX WITHHELD                           
FRSBLNK2 DS    CL11                BLANK                                        
FRSZEROS DS    CL22                ZEROS                                        
FRSETID  DS    CL10                EMPLOYER TAX ID                              
FRSETLOC DS    CL3                 EMPLOYER TAX LOCATION                        
*                                                                               
         ORG   FRSSSA                                                           
         DC    45C' '              BLANK                                        
FRSPRZR  DS    CL48'0'             ZEROS                                        
         DC    31C' '              BLANK                                        
FRSPRZR2 DS    CL34'0'             ZEROS                                        
         DC    1C' '               BLANK                                        
FRSPRZR3 DS    CL22'0'             ZEROS                                        
         DC    7C' '               BLANK                                        
FRSPREPN DS    CL10                EMPLOYER PHONE NUMBER                        
FRSPRCOD DS    CL8'0'              CEASE OF OPERATIONS DATE                     
*                                                                               
         ORG   FRSEMPAC                                                         
FRSCAEMA DS    CL8                 CALIFORNIA STATE EMPLOYER ACCT               
FRSCABRN DS    CL3'0'              CALIFORNIA BRANCH CODE                       
         DS    9C' '                                                            
*                                                                               
         ORG   FRSEMPAC                                                         
         DS    CL10                                                             
FRSFEIN  DS    CL9                 FEDERAL EMP ID NUMBER                        
         DS    CL1                                                              
*                                                                               
         DC    6C' '               BLANK                                        
FRSSTCDE DS    CL2                 STATE CODE                                   
FRSWAGES DS    CL11                WAGES, TIPS AND OTHER COMPENSATION           
FRSSTAX  DS    CL11                STATE TAX WITHHELD                           
FRSSOTHR DS    CL10                OTHER STATE DATA                             
*                                                                               
FRSFILL  DS    CL20                MD - FILLER                                  
*                                                                               
         ORG   FRSFILL                                                          
         DS    CL6                 BLANK                                        
FRSFLI   DS    CL5                 FAMILY LEAVE INSURANCE WITHHELD (NJ)         
         DS    CL9                 BLANK                                        
*                                                                               
FRSMDEIN DS    CL9                 MD - EMPLOYER ID NUMBER (EIN)                
*                                                                               
         ORG   FRSFILL                                                          
FRSLTXTY DC    CL1'C'              TAX TYPE CODE, CITY INCOME TAX               
FRSLWAGE DS    CL11                LOCAL TAXABLE WAGES                          
FRSLTAX  DS    CL11                LOCAL TAX WITHELD                            
FRSCNTRL DS    CL7                 STATE CONTROL NUMBER                         
FRSOTLN1 EQU   *-FRSSOTHR                                                       
*                                                                               
FRSSUP1  DS    CL75' '             SUPPLEMENTAL DATA 1                          
         ORG   FRSSUP1                                                          
FRSSUPKR DS    CL11                KY, RURAL ECONOMIC DEVELOPMENT               
FRSSUPKJ DS    CL11                KY, JOBS DEVELOPMENT ACT                     
FRSSUPKI DS    CL11                KY, INDUSTRIAL REVITALIZATION AUTH           
FRSSUPKD DS    CL11                KY, INDUSTRIAL DEVELOPMENT AUTH              
         DC    CL20' '                                                          
         ORG   FRSSUP1                                                          
FRSSUPKS DS    CL11                KS, KPERS, KP & F AND JUDGES                 
         ORG   FRSSUP1                                                          
FRSSUPFM DS    CL11                FICA AND MEDICARE TAX WITHHELD               
         ORG   FRSSUP1                                                          
FRSSUMSI DS    CL11                MISCELLANEOUS INCOME (1099)                  
         DS    CL44                                                             
FRSSYEAR DS    CL4                 PAYMENT YEAR                                 
         ORG   FRSSUP1                                                          
         DC    CL18' '             SPACES                                       
FRSSCNTL DS    CL9                 CONTROL NUMBER (PUERTO RICO)                 
FRSSPRZ2 DS    CL33                ZEROS                                        
         DC    CL1' '                                                           
FRSSACCD DS    CL5                 ACCESS CODE (PUERTO RICO)                    
FRSSPRZ  DS    CL46                ZEROS (PUERTO RICO)                          
         ORG   FRSSUP1                                                          
FRSMDAN  DS    CL8                 MARYLAND ACCOUNT NUMBER                      
FRSFDWG  DS    CL11                FEDERAK WAGES, TIPS, COMPENSATION            
FRSFDTX  DS    CL11                FEDERAL INCOME TAX WITHHELD                  
FRSFDEX  DS    CL2                 FEDERAL EXEMPTIONS (ON W4 RECORD)            
         ORG   FRSSUP1                                                          
         DC    CL15' '             NO TALENT PRIVATE DISABILITY                 
FRSSUPUI DS    CL5                 UNEMPLOYMENT INSURANCE                       
FRSSUPDI DS    CL5                 DISABILITY INSURANCE                         
FRSSUPPI DS    CL1                 PENSION INDICATOR                            
         DC    CL1' '              DEFERRED COMPENSATION INDICATOR              
FRSSUPDC DC    CL9' '              NO TALENT DEFERRED COMPENSATION              
         DC    CL39' '                                                          
*                                                                               
*        ARKANSAS                                                               
*                                                                               
         ORG   FRSSUP1                                                          
FRSAFEIN DC    CL9' '              FEIN FOR COMPANY                             
         DC    CL66' '                                                          
*                                                                               
*        OREGON                                                                 
*                                                                               
         ORG   FRSSUP1                                                          
FRSORAMW DS    CL11                AMTRACK WAGES                                
FRSORWWW DS    CL11                WATERWAY WAGES                               
FRSORHDW DS    CL11                HYDROELECTRIC DAM WAGES                      
FRSORIIB DS    CL11                IMPUTED INCOME BENEFITS                      
FRSORIIT DS    CL11                IMPUTED INCOME TUITION                       
         DS    CL20                SPARE                                        
*                                                                               
FRSSUP2  DS    CL75' '             SUPPLEMENTAL DATA 2                          
*                                                                               
*        PUERTO RICO                                                            
*                                                                               
         ORG   FRSSUP2+30          POSITION 442                                 
FRSESC   DC    CL57' '             SPACES  (EXEMPT SALARY CODE)                 
*                                                                               
*        ARKANSAS                                                               
*                                                                               
         ORG   FRSSUP2                                                          
FRSARID  DC    CL11' '             ARKANSAS ID NUMBER                           
         DC    CL64' '             SPACES                                       
*                                                                               
         DC    9C' '               BLANK                                        
FRSDATE  DC    8C' '               DATE - YYYYMMDD (MD ONLY)                    
FRSTIME  DC    8C' '               TIMESTAMP - HHMMSSNN (MD ONLY)               
FRSLNQ   EQU   *-FRS                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              STATE TOTAL RECORD - TYPE RST                                    
*---------------------------------------------------------------------*         
*                                                                               
FRST     DS    0CL512                                                           
FRSTID   DC    CL3'RST'            RECORD IDENTIFIER 'RST'                      
FRSTNEMP DS    CL7                 NUMBER OF EMPLOYEES                          
         DC    CL11' '                                                          
FRSTSTCD DS    CL2' '              STATE CODE                                   
         DC    C' '                                                             
FRSTUITO DS    CL14'0'             QTR UNEMPLOYMENT INS. TOTAL WAGES            
         DC    C' '                                                             
FRSTITWT DS    CL14'0'             QTR INCOME TAX WAGES                         
         DC    C' '                                                             
FRSTITWH DS    CL14'0'             QTR INCOME TAX WITHELD                       
         DC    C' '                                                             
FRSTM1EM DS    CL7'0'              MONTH 1 EMPLOYMENT                           
         DC    C' '                                                             
FRSTM2EM DS    CL7'0'              MONTH 2 EMPLOYMENT                           
         DC    C' '                                                             
FRSTM3EM DS    CL7'0'              MONTH 3 EMPLOYMENT                           
         DC    420C' '                                                          
*                                                                               
FRSTLNQ  EQU   *-FRST                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              TOTAL RECORD - TYPE RT                                 *         
*---------------------------------------------------------------------*         
*                                                                               
FRT      DS    0CL512                                                           
FRTID    DC    CL2'RT'             RECORD IDENTIFIER 'RT'                       
FRTNRW   DS    CL7                 NUMBER OF RW RECORDS                         
*                                                                               
FRTWAGES DS    CL15                WAGES, TIPS AND OTHER COMPENSATION           
FRTFTAX  DS    CL15                FEDERAL TAX WITHHELD                         
FRTSSW   DS    CL15                SOCIAL SECURITY WAGES                        
FRTSSTAX DS    CL15                SOCIAL SECURITY TAX WITHHELD                 
FRTMCW   DS    CL15                MEDICARE WAGES                               
FRTMTAX  DS    CL15                MEDICARE TAX WITHHELD                        
*                                                                               
FRTSSTIP DC    15C'0'              SOCIAL SECURITY TIPS                         
         DC    15C' '              ADVANCE EARNED INCOME CREDIT                 
FRTDCB   DC    15C'0'              DEPENDENT CARE BENEFITS                      
FRTDC401 DC    15C'0'              DEFERRED COMPENSATION TO 401(K)              
FRTDC403 DC    15C'0'              DEFERRED COMPENSATION TO 403(B)              
FRTDC408 DC    15C'0'              DEFERRED COMPENSATION TO 408(K)(6)           
FRTDC457 DC    15C'0'              DEFERRED COMPENSATION TO 457(B)              
FRTDC501 DC    15C'0'              DEFERRED COMPENSATION TO 501(C) &            
*                                  DEFERRED COMPENSATION TO 18(D)               
FRTBQSC  DC    15C' '              BASIS QUARTERS, SUBSISTENCE & COMBAT         
FRTNQ457 DC    15C'0'              NON-QUALIFIED PLAN SECTION 457               
FRTECHSA DC    15C'0'              EMP CONTRIB TO HEALTH SAVINGS ACC            
FRTNQOTH DC    15C'0'              NON-QUALIFIED PLAN NOT SECTION 457           
FRTNCPAY DC    15C'0'              NON-TAXABLE COMBAT PAY                       
*                                                                               
**NOP FRTFRNG  DS    CL15                FRINGE BENEFITS                        
FRTFRNG  DS    CL15'0'             FRINGE BENEFITS                              
*                                                                               
FRTELIFE DC    15C'0'              EMPLOYEE COST FOR LIFE > $50,000             
FRTIT3RD DC    15C'0'              INCOME TAX WITHHELD BY THIRD PARTY           
FRTXSOPT DC    15C'0'              INCOME FROM EXERCISING STOCK OPTIONS         
FRTD409A DC    15C'0'              DEFERRAL UNDER SECTION 409A                  
FRTSSA   DS    CL143' '            RESERVED FOR SSA USE                         
         ORG   FRTSSA                                                           
         DS    CL93' '                                                          
FRTDCEHC DS    CL11'0'             DC - EMPLYR HEALTH COVERAGE                  
         ORG   FRTSSA                                                           
FRT401K  DC    15C'0'              ROTH CONTRIBUTIONS TO 401K                   
FRT403B  DC    15C'0'              ROTH CONTRIBUTIONS TO 403B                   
         DS    CL22' '             SPACES                                       
FRTZERO1 DS    CL45                ZEROS                                        
FRTZERO2 DS    CL45                ZEROS                                        
         DC    CL1' '              SPACE                                        
         ORG   FRTNRW                                                           
         DS    CL337                                                            
         ORG   FRTNRW                                                           
         DS    CL292                                                            
FRTLCLWA DS    CL15'0'                                                          
         DS    CL30                                                             
FRTPASP  DS    CL136' '            SPACES FOR PA                                
FRTPARSN DS    CL7                 NUMBER OF RS RECORDS                         
FRTPAWAG DS    CL15                PA TAXABLE WAGES                             
FRTPATAX DS    CL15                PA TAX WITHHELD                              
         DS    CL1' '                                                           
FRTLNQ   EQU   *-FRT                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              TOTAL RECORD - TYPE RU                                 *         
*---------------------------------------------------------------------*         
*                                                                               
FRU      DS    0CL512                                                           
FRUID    DC    CL2'RU'             RECORD IDENTIFER  'RU'                       
FRUNRO   DS    CL7                 NUMBER OF RO RECORDS                         
FRUATIPS DC    15C'0'              ALLOCATED TIPS                               
FRUUETXT DC    15C'0'              UNCOLLECTED EMPLOYEE TAX ON TIPS             
FRUMSACC DC    15C'0'              MEDICAL SAVINGS ACCOUNT                      
FRUSRACC DC    15C'0'              SIMPLE RETIREMENT ACCOUNT                    
FRUQAEXP DC    15C'0'              QUALIFIED ADOPTION EXPENSES                  
*                                                                               
FRUUCSST DC    15C'0'              UNCOLLECTED SS OR RRTA TAX > $50,000         
FRUUCMED DC    15C'0'              UNCOLLECTED MEDICARE TAX > $50,000           
FRUI409A DC    15C'0'              INCOME UNDER SECTION 409A                    
         DC    15C' '              NEW HIRE ACT EXEMPT WAGES                    
**NOP    DC    210C' '             RESERVED FOR SSA USE                         
         DC    15C'0'                                                           
         DC    195C' '             RESERVED FOR SSA USE                         
*                                                                               
FRUWGPRT DS    CL15                WAGES SUBJECT TO PUERTO RICO TAX             
FRUCMPRT DC    15C'0'              COMMISSIONS SUBJECT TO PR TAX                
FRUALPRT DC    15C'0'              ALLOWANCES SUBJECT TO PR TAX                 
FRUTPPRT DC    15C'0'              TIPS SUBJECT TO PUERTO RICO TAX              
FRUTWPRT DS    CL15                TOTAL WAGES, COMM, TIPS, AND ALLOW           
*                                  SUBJECT TO PUERTO RICO TAX                   
FRUPRTWT DS    CL15                PUERTO RICO TAX WITHHELD                     
FRURFAC  DC    15C'0'              RETIREMENT FUND ANNUAL CONTRIBUTIONS         
*                                                                               
FRUTWVI  DC    15C'0'              TOTAL WAGES, TIPS AND OTHER TO VI            
FRUVIWT  DC    15C'0'              VIRGIN ISLAND TAX WITHHELD                   
         DC    23C' '              RESERVED FOR SSA USE                         
FRULNQ   EQU   *-FRU                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              TOTAL RECORD - TYPE RV                                 *         
*---------------------------------------------------------------------*         
*                                                                               
FRV      DS    0CL512                                                           
FRVID    DC    CL2'RV'             RECORD IDENTIFIER 'RV'                       
FRVPREPN DS    CL10                EMPLOYER PHONE NUMBER (PUERTO RICO)          
FRVSACCD DS    CL5                 ACCESS CODE (PUERTO RICO)                    
FRVSPRZ  DS    CL45                ZEROS (PUERTO RICO)                          
FRVSPRZ2 DS    CL45                ZEROS (PUERTO RICO)                          
FRVSPRZ3 DS    CL30                ZEROS (PUERTO RICO)                          
         DC    390C' '             BLANK                                        
*                                                                               
         ORG   FRVPREPN                                                         
FRVCNT   DS    CL7                 RS RECORD COUNT                              
FRVWGS   DS    CL15                TOTAL WAGES                                  
FRVTXWT  DS    CL15                TOTAL TAX WITHHELD                           
*                                                                               
         ORG   FRVPREPN                                                         
FRVSCOD2 DS    CL2                 STATE CODE (INDIANA)                         
FRVCNT4  DS    CL13                TOTAL # OF RS RECORDS                        
FRVTXWT3 DS    CL13                TOTAL TAX WITHHELD                           
FRVCTWT3 DS    CL13                TOTAL COUNTY TAX WITHHELD                    
*                                                                               
         ORG   FRVPREPN                                                         
FRVSCOD5 DS    CL2                 STATE CODE (OKLAHOMA)                        
FRVCNT5  DS    CL7                 RS RECORD COUNT                              
FRVEIN5  DS    CL9                 EMPLOYER ID NUMBER (EIN)                     
         DS    CL21                                                             
FRVSWGS  DS    CL15                STATE WAGES                                  
FRVSTXWT DS    CL15                STATE TAX WITHHELD                           
FRVLWGS  DS    CL15                LOCAL WAGES                                  
FRVLTXWT DS    CL15                LOCAL TAX WITHHELD                           
*                                                                               
         ORG   FRVPREPN                                                         
FRVEIN2  DS    CL9                 EMPLOYER ID NUMBER (EIN)  (IDAHO)            
FRVENAM2 DS    CL4                 EMPLOYER NAME (1ST 4 CHARS)                  
FRVSACC  DS    CL9                 STATE ACCOUNT NUMBER                         
FRVFCYC  DS    CL1                 FILING CYCLE                                 
FRVTPER  DS    CL2                 TAX PERIOD (MM)                              
FRVTYEAR DS    CL4                 TAX YEAR (YYYY)                              
FRVWGS2  DS    CL11                TOTAL WAGES                                  
FRVTXWT2 DS    CL11                TOTAL TAX WITHHELD                           
FRVTWITH DS    CL11                TOTAL WITHHOLDING PAYMENTS                   
         DS    CL11                BLANK                                        
         DS    CL11                BLANK                                        
FRVRTAX  DS    CL11                REMAINING TAX DUE OR REFUND                  
FRVTDSN  DS    CL1                 TAX DUE SIGN                                 
FRVPNLTY DS    CL11                PENALTY ON BALANCE DUE                       
FRVINT   DS    CL11                INTEREST ON BALANCE DUE                      
FRVTAMTD DS    CL11                TOTAL AMOUNTS DUE OR REFUND                  
FRVTDSN2 DS    CL1                 TAX DUE SIGN                                 
FRVCNT2  DS    CL7                 TOTAL NUMBER OF W2'S                         
FRVCNT3  DS    CL7                 TOTAL NUMBER OF 1099'S                       
FRVFSC   DS    CL1                 FED/STATE COMBINED Y=1, N=0                  
FRVTSTAT DS    CL7                 TOTAL NUMBER OF STATEMENTS                   
FRVPNLT2 DS    CL11                PENALTY FOR LATE FILING                      
FRVTDRF  DS    CL11                TOTAL DUE OR REFUND                          
FRVTDSN3 DS    CL1                 TOTAL DUE SIGN                               
*                                                                               
         ORG   FRVPREPN                                                         
FRVSCODE DS    CL2' '              STATE CODE                                   
FRVSRTYP DS    CL5                 STATE RECORD TYPE                            
FRVYR    DS    CL4                 TAX YEAR                                     
FRVEIN   DS    CL9                 EMPLOYER ID NUMBER (EIN)                     
FRVCRN   DC    CL8' '              8 DIGIT CENTRAL REGISTRATION NUMBER          
*                                                                               
FRVENAM  DC    CL57' '             EMPLOYER NAME                                
FRVLADD  DS    CL22' '             LOCATION ADDRESS                             
FRVCITY  DS    CL22' '             CITY                                         
FRVSTAT  DS    CL2                 STATE ABBREVIATION                           
FRVZIP   DS    CL5                 ZIP CODE                                     
FRVZIPX  DS    CL4                 ZIP CODE EXTENSION                           
*                                                                               
FRVTOTRS DS    CL6                 NUMBER OF W2S                                
*                                                                               
FRVTTAX  DS    CL12                TOTAL TAX REPORTED                           
FRVTAXW  DS    CL12                TOTAL TAX WITHHELD                           
FRVEXEM  DS    CL12                TOTAL TAX EXEMPT CREDITS                     
FRVTXDUE DS    CL12                AMOUNT OF TAX DUE                            
FRVOPYM5 DS    CL12                OVERPAYMENT FROM LINE 5                      
FRVOPYM6 DS    CL12                OVERPAYMENT FROM LINE 6                      
FRVOPCRD DS    CL12                AMOUNT OF OVERPYMT APPLIED AS CREDIT         
FRVOPRFD DS    CL12                AMOUNT OF OVERPYMT TO BE REFUNDED            
FRVWAGES DS    CL12                WAGES, TIPS AND OTHER COMPENSATION           
FRVPICKP DS    CL12                STATE PICKUP AMOUNT                          
*                                                                               
FRVCTNAM DS    CL28                CONTACT NAME                                 
FRVCTTIT DS    CL15                CONTACT TITLE                                
FRVDATE  DS    CL8                 DATE REPORT IS SUBMITTED                     
FRVCTPH  DS    CL10                CONTACT PHONE NUMBER                         
*                                                                               
FRVTFILE DC    CL1'Y'              TOTAL FILE INDICATOR                         
FRVTW2   DS    CL1                 WILL YOU SUBMIT ADDTL W2S?                   
FRVT1099 DS    CL1                 WILL YOU SUBMIT ADDTL 1099?                  
FRVNAICS DS    CL6                 NAICS CODE                                   
         DS    CL158               BLANK                                        
FRVDATE2 DS    CL8                 DATE CREATED - YYYYMMDD (MD ONLY)            
FRVTIME  DS    CL8                 TIME CREATED - HHMMSSNN (MD ONLY)            
FRVLNQ   EQU   *-FRV                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              FINAL RECORD - TYPE RF                                 *         
*---------------------------------------------------------------------*         
*                                                                               
FRF      DS    0CL512                                                           
FRFID    DC    CL2'RF'             RECORD IDENTIFIER 'RF'                       
FRFSSA   DC    CL5' '              RESERVED FOR SSA USE                         
FRFNRW   DS    CL9                 NUMBER OF RW RECORDS                         
         ORG   FRFNRW                                                           
FRFNRWKY DS    CL6                 KENTUCKY IS 6                                
         DS    CL3                                                              
         DC    496C' '             RESERVED FOR SSA USE                         
         ORG   FRFNRW                                                           
         DS    CL468                                                            
FRFRSN   DS    CL7                 NUMBER OF RS RECORDS                         
FRFWAGE  DS    CL15                TOTAL TAXABLE WAGES                          
FRFTAX   DS    CL15                TOTAL TAX WITHHELD                           
         ORG   FRFSSA                                                           
FRFRSN2  DS    CL9                 NUMBER OF RS RECORDS (CT)                    
FRFWAGE2 DS    CL16                TOTAL TAXABLE WAGES (CT)                     
FRFTAX2  DS    CL16                TOTAL TAX WITHHELD (CT)                      
         DS    CL469                                                            
         EJECT                                                                  
         ENTRY W2TAPE                                                           
W2TAPE   DCB   DDNAME=W2TAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=FB,LRECL=512,BUFNO=2,BLKSIZE=23040                         
*                                                                               
         ENTRY TADOWN                                                           
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=512,BUFNO=2,BLKSIZE=23040,EODAD=NOMORE            
         EJECT                                                                  
* TASYSCATS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSCATS                                                      
         PRINT ON                                                               
         EJECT                                                                  
*=====================================================================*         
*              PUERTO RICO WORKER?                                              
*=====================================================================*         
GPVWRKER NTR1  BASE=*,LABEL=*                                                   
         MVI   GPVWRK,C'N'                                                      
         MVC   GPVSTATE,=C'  '                                                  
*                                                                               
         L     R6,TIAREC           LOOK IN W2 RECORD TO SEE                     
         MVI   ELCODE,TAW2ELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GPV100   BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'PR '    PUERTO RICO                                  
         BE    GPV200                                                           
         CLC   TAW2UNIT,=C'GU '    GUAM                                         
         BE    GPV200                                                           
         CLC   TAW2UNIT,=C'VI '    VIRGIN ISLAND                                
         BE    GPV200                                                           
         B     GPV100                                                           
*                                                                               
GPV200   MVI   GPVWRK,C'Y'         WORKED IN GU, PR, OR VI = YES                
         MVC   GPVSTATE,TAW2UNIT                                                
         J     YES                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*              LISTING SUBSIDIARY PRINT ROUTINES                                
*                                                                               
*                                  R3=A(ACCUMULATOR SET)                        
         USING ACCUMD,R3                                                        
FORMAT   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,MYP                                                           
         USING PRINTD,R4                                                        
         CLI   TAPETYPE,FEDERAL                                                 
         BNE   FORMATST                                                         
         EDIT  FEDWAGES,(12,PRWAGES),2,ZERO=BLANK                               
         EDIT  FEDTAXES,(12,PRTAXES),2,ZERO=BLANK                               
         EDIT  FICWAGES,(12,PRFICWAG),2,ZERO=BLANK                              
         EDIT  FICTAXES,(12,PRFICA),2,ZERO=BLANK                                
         EDIT  MEDWAGES,(12,PRMEDWAG),2,ZERO=BLANK                              
         EDIT  MEDTAXES,(12,PRMEDCAR),2,ZERO=BLANK                              
         EDIT  FEDREXP,(12,PRFB),2,ZERO=BLANK                                   
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DIFFERENT VALUES FOR STATES                                      
*                                                                               
FORMATST EDIT  FEDWAGES,(12,PRWAGES),2,ZERO=BLANK                               
         EDIT  FEDTAXES,(12,PRTAXES),2,ZERO=BLANK                               
         EDIT  STAWAGES,(12,PRSTAWAG),2,ZERO=BLANK                              
         EDIT  STATAXES,(12,PRSTATAX),2,ZERO=BLANK                              
         EDIT  STASDI,(12,PRSDI),2,ZERO=BLANK                                   
         CLC   TIFUNIT(2),=C'NJ'                                                
         BE    FORMATNJ                                                         
         EDIT  LOCWAGES,(12,PRLOCWAG),2,ZERO=BLANK                              
         EDIT  LOCTAXES,(12,PRLOCTAX),2,ZERO=BLANK                              
         J     XIT                                                              
*                                                                               
FORMATNJ EDIT  STASUI,(12,PRSUI),2,ZERO=BLANK                                   
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*              SUBSIDIARY ROUTINES - LOCAL LIST                                 
*                                                                               
ADDLOCAL NTR1  BASE=*,LABEL=*                                                   
         L     R6,TIAREC           ADD LOCALS TO STATE LIST                     
         MVI   ELCODE,TAW2ELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
ALOC2    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'FD '                                                 
         BE    ALOC2                                                            
         CLI   TAW2UNIT+2,C'A'                                                  
         BL    ALOC2                                                            
         CLC   =C'LAX',TAW2UNIT    SKIP LAX (FAKE CA CITY)                      
         BE    ALOC2                                                            
         GOTO1 TAXVAL,DMCB,(3,TAW2UNIT)                                         
         LA    R2,UNITLIST         LOOK FOR MATCHING STATE FOR LOCAL            
*                                                                               
ALOC4    CLI   0(R2),0                                                          
         BE    ALOC2               (NOT FOUND - IGNORE)                         
         CLC   0(3,R2),TGTASTCY                                                 
         BE    ALOC6                                                            
         LA    R2,6(R2)                                                         
         B     ALOC4                                                            
*                                                                               
ALOC6    MVC   3(3,R2),TAW2UNIT    ADD LOCAL TO UNITLIST AFTER STATE            
         B     ALOC2                                                            
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
         USING FRW,R2                                                           
         USING ACCUMD,R3                                                        
XTRCTRW  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,FEDACCS                                                       
         CLI   FEDPRSW,1           SECONDARY?                                   
         BE    XTRCTRW5                                                         
         PACK  FICWAGES,FRWSSW     SOCIAL SECURITY WAGES                        
         PACK  FICTAXES,FRWSSTAX   SOCIAL SECURITY TAX WITHHELD                 
         PACK  MEDWAGES,FRWMCW     MEDICARE WAGES                               
         PACK  MEDTAXES,FRWMTAX    MEDICARE TAX WITHHELD                        
         J     XIT                                                              
*                                                                               
XTRCTRW5 PACK  TPRFWAGE,FRWSSW     SOCIAL SECURITY WAGES                        
         PACK  TPRFTAX,FRWSSTAX    SOCIAL SECURITY TAX WITHHELD                 
         PACK  TPRMWAGE,FRWMCW     MEDICARE WAGES                               
         PACK  TPRMTAX,FRWMTAX     MEDICARE TAX WITHHELD                        
*                                                                               
XTRCTRWX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
         USING FRO,R2                                                           
XTRCTRO  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,FEDACCS                                                       
         PACK  TPRWAGES,FROTWPRT   PUERTO RICO WAGES                            
         PACK  TPRTAXES,FROPRTWT   PUERTO RICO TAXES                            
*        PACK  TPRNHAWG,FRONHAWG   NHA EXEMPT WAGES                             
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
PRTOTS   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,EMPACCS                                                       
         USING ACCUMD,R3                                                        
         CP    TPRWAGES,=P'0'      WAGES FROM PUERTO RICO                       
         JE    XIT                                                              
         BRAS  RE,SPLAT                                                         
         USING PRINTD,R2                                                        
         LA    R2,MYP1                                                          
         MVC   PRNAME,=CL30'*** PUERTO RICO ***'                                
         EDIT  TPRWAGES,(12,PRWAGES),2,ZERO=BLANK                               
         EDIT  TPRFWAGE,(12,PRFICWAG),2,ZERO=BLANK                              
         EDIT  TPRFTAX,(12,PRFICA),2,ZERO=BLANK                                 
         EDIT  TPRMWAGE,(12,PRMEDWAG),2,ZERO=BLANK                              
         EDIT  TPRMTAX,(12,PRMEDCAR),2,ZERO=BLANK                               
         BRAS  RE,SPLAT                                                         
         BRAS  RE,SPLAT                                                         
         J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              GET AND INITIALIZE BUFFER                              *         
*                             BUFFER  =  BYTES  1-4 N'ENTRIES         *         
*                             BYTES  5-8 L'ENTRY                      *         
*                             BYTES 9-12 NUMBER OF LAST ENTRY         *         
*                             BYTES 13+  THE BUFFER!                  *         
***********************************************************************         
*                                                                               
INITBUFF NTR1  BASE=*,LABEL=*                                                   
         L     R2,ASPFUSER         SAVE A(BUFFER) IN USER AREA                  
         USING SPFUSERD,R2                                                      
         OC    ABUFFER,ABUFFER     IF NO ADDRESS                                
         BNZ   INITB10                                                          
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)         GET THE SPACE                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER          AND SAVE THE ADDRESS                         
*                                                                               
INITB10  L     R1,ABUFFER                                                       
         MVC   0(4,R1),=F'150'     SET UP FOR 150 RECORDS                       
         MVC   4(4,R1),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R1),8(R1)                                                    
         LA    RF,150                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R1)           CLEAR THE BUFFER                             
         XCEFL                                                                  
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FOR CITIES - CHECK IF WAGES AND TAXES ARE ZERO                         
***********************************************************************         
         USING ACCUMD,R3                                                        
CITYCHK  NTR1  BASE=*,LABEL=*                                                   
         CLI   STATUNIT+2,X'40'    CITIES HAVE A 3RD CHARACTER                  
         BNH   CCHKYES                                                          
         CP    STAWAGES,=P'0'      ANY WAGES?                                   
         BNE   CCHKYES                                                          
         CP    STATAXES,=P'0'      ANY TAXES                                    
         BE    CCHKNO                                                           
*                                                                               
CCHKYES  XR    RC,RC                                                            
CCHKNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
***********************************************************************         
*                                                                               
NEWPRTQ2 NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPRT10                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPRT10   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPRT20                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPRT30                                                           
*                                                                               
NPRT20   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TWE'                                                 
NPRT30   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'W2DATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         XIT1                                                                   
         DROP  R2,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ADJUST FEDERAL WAGE FOR GUAM, PR OR VI WORKERS         *         
***********************************************************************         
GPVADJW  NTR1  BASE=*,LABEL=*                                                   
         USING TAW2D,R6                                                         
         USING ACCUMD,R3                                                        
         L     R4,=A(LIMBLOCK)                                                  
         USING TMD,R4                                                           
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GPVA100  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLC   TAW2UNIT,=C'GU '           GUAM                                  
         BE    GPVA200                                                          
         CLC   TAW2UNIT,=C'PR '           PUERTO RICO                           
         BE    GPVA200                                                          
         CLC   TAW2UNIT,=C'VI '           VIRGIN ISLANDS                        
         BNE   GPVA100                                                          
GPVA200  L     R2,TAW2EARN                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R2,TAW2TXRE         ADD TAXABLE REIMBURSEMENTS                   
         CVD   R2,WORK                                                          
         CP    FEDWAGES,WORK(8)    ALL PUERTO RICO?                             
         BNE   GPVA300                                                          
*        BE    GPVA250                                                          
*        ZAP   DUB,WORK(8)                                                      
*        SP    FEDWAGES,                                                        
*        CP    FEDWAGES,DUB                                                     
*        BNE   GPVA300                                                          
*                                                                               
GPVA250  ZAP   CPRWAGES,FEDWAGES   MOVE OUT THE FED PORTION                     
         ZAP   CPRFWAGE,FICWAGES   TO PUERTO RICO                               
         ZAP   CPRFTAX,FICTAXES                                                 
         ZAP   CPRMWAGE,MEDWAGES                                                
         ZAP   CPRMTAX,MEDTAXES                                                 
         ZAP   FEDWAGES,=P'0'      ZERO OUT FED PORTION                         
         ZAP   FICWAGES,=P'0'                                                   
         ZAP   FICTAXES,=P'0'                                                   
         ZAP   MEDWAGES,=P'0'                                                   
         ZAP   MEDTAXES,=P'0'                                                   
*        ZAP   NHAWAGES,=P'0'                                                   
         B     GPVAX                                                            
*                                                                               
GPVA300  SP    FEDWAGES,WORK(8)                                                 
         SP    FICWAGES,WORK(8)                                                 
         SP    MEDWAGES,WORK(8)                                                 
*                                                                               
         ZAP   CPRWAGES,WORK(8)                                                 
         ZAP   CPRFWAGE,WORK(8)                                                 
         ZAP   CPRMWAGE,WORK(8)                                                 
*                                                                               
         CVB   R1,FEDWAGES         UPDATE FICA AND MEDICARE WAGES & TAX         
         CVB   RF,FEDREXP          EXCLUDE REIMBURSED EXPENSES                  
         SR    R1,RF               EXCLUDE REIMBURSED EXPENSES                  
         ST    R1,TMTFICA                                                       
         ST    R1,TMTMED                                                        
         ST    R1,TMTTOTAL                                                      
*                                                                               
         GOTO1 =V(TALIM),DMCB,(R4)                                              
*                                                                               
         L     R1,TMXFICA                                                       
         CVD   R1,DUB                                                           
         ZAP   WORK(8),DUB                                                      
         ZAP   CPRFTAX,FICTAXES                                                 
         SP    CPRFTAX,WORK(8)                                                  
         ZAP   FICTAXES,WORK(8)                                                 
         L     R1,TMXMED                                                        
         CVD   R1,DUB                                                           
         ZAP   WORK(8),DUB                                                      
         ZAP   CPRMTAX,MEDTAXES                                                 
         SP    CPRMTAX,WORK(8)                                                  
         ZAP   MEDTAXES,WORK(8)                                                 
*                                                                               
GPVAX    B     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        MMREF EMPLOYER WAGE RECORD - TYPE 'RO'                       *         
*              FEDERAL AND PUERTO RICO ONLY                           *         
***********************************************************************         
*                                                                               
PUTOM    NTR1  BASE=*,LABEL=*                                                   
         L     R2,=A(FRO)                                                       
         USING FRO,R2                                                           
         LA    R3,FEDACCS                                                       
         USING ACCUMD,R3                                                        
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         MVC   FROWGPRT(11),ALLZERO3                                            
         MVC   FROTWPRT(11),ALLZERO3                                            
*        MVC   FRONHAWG(11),ALLZERO3                                            
         MVC   FROPRTWT(11),ALLZERO3                                            
*        CLC   TIFYEAR(2),=C'10'                                                
*        BNE   PUTOM030                                                         
*        CP    W2NHAWG,=P'0'                                                    
*        BE    PUTOM030                                                         
*        EDIT  W2NHAWG,(11,FRONHAWG),0,ZERO=NOBLANK,FILL=0                      
*                                                                               
PUTOM030 MVI   FROCSTAT,C' '       BLANK FOR PUERTO RICO                        
         CLC   STATUNIT,=C'PR '                                                 
         BE    PUTOM100                                                         
         MVI   FROCSTAT,C'S'       CIVIL STATUS SINGLE (DEFAULT)                
         MVC   FROSPSSN,MYSPACES                                                
         USING TAWHD,R6                                                         
         L     R6,ATHISW4                                                       
         MVI   ELCODE,TAWHELQ      WITHHOLDING ELEMENT                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PUTOM050 BAS   RE,NEXTEL                                                        
         BNE   PUTOM100                                                         
         CLC   TAWHUNIT,=C'PR '                                                 
         BNE   PUTOM050                                                         
         MVC   FROCSTAT,TAWHSTAT                                                
*                                                                               
         USING TAW2D,R6                                                         
PUTOM100 L     R6,TIAREC                                                        
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PUTOM150 BAS   RE,NEXTEL                                                        
         BNE   PUTOM200                                                         
         CLC   TAW2UNIT,=C'PR '                                                 
         BNE   PUTOM150                                                         
         L     R1,TAW2EARN                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         ADD ANY TAXABLE REIMBURSEMENTS               
         EDIT  (R1),(11,FROWGPRT),0,ZERO=NOBLANK,FILL=0                         
         EDIT  (R1),(11,FROTWPRT),0,ZERO=NOBLANK,FILL=0                         
**NO-OP  EDIT  TAW2EARN,(11,FROWGPRT),0,ZERO=NOBLANK,FILL=0                     
**12/14  EDIT  TAW2EARN,(11,FROTWPRT),0,ZERO=NOBLANK,FILL=0                     
         EDIT  TAW2TAX,(11,FROPRTWT),0,ZERO=NOBLANK,FILL=0                      
*                                                                               
         L     RF,=A(SRTRO)                                                     
         USING SRTRO,RF                                                         
         MVC   SRTROST,GPVSTATE                                                 
         MVC   SRTROSS,TLW2SSN                                                  
         MVI   SRTRORC,2                                                        
         DROP  RF                                                               
*                                                                               
         LA    R3,EMPACCS                                                       
         L     R1,TAW2EARN                                                      
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    *+8                                                              
         A     R1,TAW2TXRE         ADD ANY TAXABLE REIMBURSEMENTS               
         CVD   R1,WORK                                                          
         AP    TPRWAGES,WORK(8)                                                 
         L     R1,TAW2TAX                                                       
         CVD   R1,WORK                                                          
         AP    TPRTAXES,WORK(8)                                                 
         DROP  R6                                                               
*                                                                               
PUTOM200 CLI   TAPETYPE,FEDERAL    FEDERAL?                                     
         BE    PUTOM900                                                         
         CLC   STATUNIT,=C'PR '                                                 
         BNE   PUTOM900                                                         
         MVC   FRORFAC,ALLZERO3    FILL WITH ZEROS                              
         MVC   FROTWVI,ALLZERO3    FILL WITH ZEROS                              
         MVC   FROVIWT,ALLZERO3    FILL WITH ZEROS                              
         MVC   FROSPSSN,MYSPACES                                                
         BAS   RE,PUTTAPEO                                                      
         B     XIT                                                              
*                                                                               
PUTOM900 CLI   TAPETYPE,FEDERAL    FEDERAL?                                     
         BNE   XIT                                                              
         CLI   GPVWRK,C'Y'                                                      
         BE    PUTOM950                                                         
*&&DO                                                                           
         CLC   TIFYEAR(2),=C'10'                                                
         BNE   XIT                                                              
         CP    W2NHAWG,=P'0'                                                    
         BE    XIT                                                              
         BAS   RE,PUTTAPEO         PUT OUT RO REC                               
         BRAS  RE,XTRCTRO          EXTRACT RO AMOUNTS                           
         BAS   RE,FEDTOTS                                                       
*&&                                                                             
         B     XIT                                                              
*                                                                               
PUTOM950 L     RF,=A(SRTRO)                                                     
         GOTO1 SORTER,DMCB,=C'PUT',(RF)                                         
         AP    SRTCOUNT,=P'1'                                                   
         B     XIT                 YES, LEAVE                                   
         DROP  R2,R4                                                            
*                                                                               
         LTORG                                                                  
ALLZERO3 DC    C'000000000000000000000000000000000000000000000000' 48ZS         
         EJECT                                                                  
***********************************************************************         
*        MMREF EMPLOYER WAGE RECORD - TYPE 'RU'                       *         
*              PUERTO RICO ONLY                                       *         
***********************************************************************         
*                                                                               
PUTU     NTR1  BASE=*,LABEL=*                                                   
         L     R2,=A(FRU)                                                       
         USING FRU,R2                                                           
         LA    R3,EMPACCS                                                       
         USING ACCUMD,R3                                                        
*        MVC   FRUNHAWG(15),=15C'0'                                             
         MVC   FRUWGPRT(15),=15C'0'                                             
         MVC   FRUTWPRT(15),=15C'0'                                             
         MVC   FRUPRTWT(15),=15C'0'                                             
*        EDIT  TPRNHAWG,(15,FRUNHAWG),0,ZERO=NOBLANK,FILL=0                     
         CLI   TAPETYPE,FEDERAL                                                 
         BNE   PUTU10                                                           
         CLC   PRVSTATE,=C'  '     1ST TIME FEDERAL NO PR WAGES                 
         BNH   PUTU50                                                           
PUTU10   EDIT  TPRWAGES,(15,FRUWGPRT),0,ZERO=NOBLANK,FILL=0                     
         EDIT  TPRWAGES,(15,FRUTWPRT),0,ZERO=NOBLANK,FILL=0                     
         EDIT  TPRTAXES,(15,FRUPRTWT),0,ZERO=NOBLANK,FILL=0                     
PUTU50   UNPK  FRUNRO,ROCNT        NUMBER OF RO RECORDS                         
         OI    FRUNRO+6,X'F0'                                                   
*                                                                               
         BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MMREF TOTAL RECORD - TYPE 'V'                                *         
*        R2--> A(FRV)                                                 *         
*        RF--> STATE CODE                                             *         
***********************************************************************         
PUTRV    NTR1  BASE=*,LABEL=*                                                   
         LA    R3,EMPACCS                                                       
         USING ACCUMD,R3                                                        
         USING FRV,R2                                                           
*                                                                               
         CLC   TIFUNIT,=C'ID '     IF IDAHO,                                    
         BE    PRV50               SKIP THIS                                    
*                                                                               
         CLC   TIFUNIT,=C'IN '     IF INDIANA,                                  
         BE    PRV60               SKIP THIS                                    
*                                                                               
         CLC   TIFUNIT,=C'OK '     IF OKLAHOMA,                                 
         BE    PRV70               SKIP THIS                                    
*                                                                               
         CLC   TIFUNIT,=C'PR '     IF PUERTO RICO,                              
         BNE   PRV20                                                            
         MVC   FRVPREPN,=C'3129237900'  EMPLOYER PHONE NUMBER                   
         MVC   FRVSACCD,=C'YA547'  ACCESS CODE                                  
         MVC   FRVSPRZ,ALLZERO4                                                 
         MVC   FRVSPRZ2,ALLZERO4                                                
         MVC   FRVSPRZ3,ALLZERO4                                                
         J     XIT                                                              
*                                                                               
PRV20    CLC   TIFUNIT,=C'MT '     IF MONTANA,                                  
         BE    *+10                                                             
         CLC   TIFUNIT,=C'IL '     IF ILLINOIS                                  
         BE    *+10                                                             
         CLC   TIFUNIT,=C'NE '     IF NEBRASKA                                  
         BE    *+10                                                             
         CLC   TIFUNIT,=C'IN '     IF INDIANA                                   
         BE    *+10                                                             
         CLC   TIFUNIT,=C'OR '     IF OREGON                                    
         BNE   PRV30                                                            
         MVC   FRVLADD(L'FRVLADD),MYSPACES                                      
         MVC   FRVCITY,MYSPACES                                                 
         MVI   FRVTFILE,C' '                                                    
         UNPK  FRVCNT,TAPSCNT      NUMBER OF RS RECORDS (W2S)                   
         OI    FRVCNT+6,X'F0'                                                   
         UNPK  FRVWGS,STAWAGES     STATE WAGES                                  
         OI    FRVWGS+14,X'F0'                                                  
         UNPK  FRVTXWT,STATAXES    STATE TAX WITHHELD                           
         OI    FRVTXWT+14,X'F0'                                                 
         J     XIT                                                              
*                                                                               
         USING STACDD,RF                                                        
PRV30    MVC   FRVSCODE,STACDE     STATE CODE                                   
         DROP  RF                                                               
*                                                                               
         MVC   FRVSRTYP,=C'MW508'  STATE RECORD TYPE                            
         MVC   FRVYR,YEAR          TAX YEAR                                     
         MVC   FRVEIN,EMPFDID      EMPLOYER ID NUMBER                           
         MVC   FRVCRN,TIDID        CENTRAL REGISTRATION NUMBER                  
*                                                                               
         MVC   FRVENAM(L'EMPNAME),EMPNAME   EMPLOYER NAME                       
         MVC   FRVLADD,TADCADD              LOCATION ADDRESS                    
         MVC   FRVCITY,TADCCITY             CITY                                
         MVC   FRVSTAT,TADCSTAT             STATE                               
         MVC   FRVZIP,TADCZIP               ZIP CODE                            
*                                                                               
         MVC   FRVZIPX,TADCZIP+6            ZIP CODE EXTENSION                  
         CLC   TIFUNIT,=C'MD '     IF MARYLAND                                  
         JNE   PRV34                                                            
PRV32    MVC   FRVZIPX,=C'3597'                                                 
*                                                                               
PRV34    UNPK  FRVTOTRS,TAPSCNT    NUMBER OF RS RECORDS (W2S)                   
         OI    FRVTOTRS+5,X'F0'                                                 
*                                                                               
         UNPK  FRVTTAX,STATAXES    STATE TAX WITHHELD                           
         OI    FRVTTAX+11,X'F0'                                                 
*                                                                               
         UNPK  FRVTAXW,STATAXES    STATE TAX WITHHELD                           
         OI    FRVTAXW+11,X'F0'                                                 
*                                                                               
         MVC   FRVEXEM,ALLZERO4    TAX EXEMPT CREDITS                           
*                                                                               
         UNPK  FRVTXDUE,STATAXES   STATE TAX WITHHELD                           
         OI    FRVTXDUE+11,X'F0'                                                
*                                                                               
         CLC   TIFUNIT,=C'MD '     IF MARYLAND,                                 
         BNE   PRV40                                                            
         UNPK  FRVEXEM,STATAXES    TOTAL WITHHOLDING TAX PAID                   
         OI    FRVEXEM+11,X'F0'                                                 
         MVC   FRVTXDUE,ALLZERO4   TOTAL TAX EXEMPT CREDITS                     
         GOTO1 DATCON,DMCB,(5,0),(20,FRVDATE2)  DATE                            
         LA    R6,FRVTIME                                                       
         BRAS  RE,GETTMSTP         GET TIMESTAMP                                
*                                                                               
PRV40    MVC   FRVOPYM5,ALLZERO4   OVERPAYMENT FROM LINE 5                      
         MVC   FRVOPYM6,ALLZERO4   OVERPAYMENT FROM LINE 6                      
         MVC   FRVOPCRD,ALLZERO4   OVERPAYMENT APPLIED AS CREDIT                
         MVC   FRVOPRFD,ALLZERO4   OVERPAYMENT REFUNDED                         
*                                                                               
         UNPK  FRVWAGES,STAWAGES   STATE WAGES                                  
         OI    FRVWAGES+11,X'F0'                                                
*                                                                               
         MVC   FRVPICKP,ALLZERO4   STATE PICKUP                                 
*                                                                               
         MVC   FRVCTNAM,=CL28'DANIELLE KOSCHIL'                                 
         CLC   TIFUNIT,=C'MD '                                                  
         BNE   PRV45                                                            
         MVC   FRVCTNAM,=CL28'JULIE HEGELMANN'                                  
         MVI   FRVTW2,C'N'         SUBMITTING ADDTL W2S?                        
         MVI   FRVT1099,C'N'       SUBMITTING ADDTL 1099S?                      
         MVC   FRVNAICS,=C'561490' NAICS CODE                                   
PRV45    MVC   FRVCTTIT,=CL15'SR TAX MANAGER'                                   
         GOTO1 DATCON,DMCB,(5,0),(20,FRVDATE)                                   
         MVC   FRVCTPH(10),=C'3129237900'                                       
         J     XIT                                                              
*                                                                               
*                                  IDAHO                                        
PRV50    MVC   FRVEIN2,EMPFDID     EMPLOYER ID NUMBER                           
         MVC   FRVENAM2,EMPNAME    EMPLOYER NAME                                
         MVC   FRVSACC,TIDID       STATE ACCOUNT NUMBER                         
         MVI   FRVFCYC,C'Q'        FILING CYCLE = QUARTERLY                     
         MVC   FRVTPER,=CL2'12'    REPORTING PERIOD MM                          
         MVC   FRVTYEAR,YEAR       TAX YEAR YYYY                                
         UNPK  FRVWGS2,STAWAGES    STATE WAGES                                  
         OI    FRVWGS2+10,X'F0'                                                 
         UNPK  FRVTXWT2,STATAXES   TOTAL STATE TAX WITHHELD                     
         OI    FRVTXWT2+10,X'F0'                                                
         UNPK  FRVTWITH,STATAXES   TOTAL WITHHOLDING PAYMENTS                   
         OI    FRVTWITH+10,X'F0'                                                
         MVC   FRVRTAX,ALLZERO4                                                 
         MVI   FRVTDSN,C' '        TAX DUE SIGN                                 
         MVC   FRVPNLTY,ALLZERO4                                                
         MVC   FRVINT,ALLZERO4                                                  
         MVC   FRVTAMTD,ALLZERO4                                                
         MVI   FRVTDSN2,C' '       TAX DUE SIGN                                 
         UNPK  FRVCNT2,TAPSCNT     NUMBER OF RS RECORDS (W2S)                   
         OI    FRVCNT2+6,X'F0'                                                  
         MVC   FRVCNT3,ALLZERO4    NUMBER OF 1099S                              
         MVI   FRVFSC,C'0'         FED/STATE COMBINED = NO                      
         MVC   FRVTSTAT,FRVCNT2    TOTAL NUMBER OF STATMENTS                    
         MVC   FRVPNLT2,ALLZERO4                                                
         MVC   FRVTDRF,ALLZERO4                                                 
         MVI   FRVTDSN3,C' '       TAX DUE SIGN                                 
         MVI   FRVTFILE,C' '       BLANK                                        
         J     XIT                                                              
*                                                                               
*                                  INDIANA                                      
         USING STACDD,RF                                                        
PRV60    MVC   FRVSCOD2,STACDE     STATE CODE                                   
         DROP  RF                                                               
         UNPK  FRVCNT4,TAPSCNT     NUMBER OF RS RECORDS                         
         OI    FRVCNT4+12,X'F0'                                                 
         UNPK  FRVTXWT3,STATAXES   TOTAL STATE TAX WITHHELD                     
         OI    FRVTXWT3+12,X'F0'                                                
         MVC   FRVCTWT3,ALLZERO4   TOTAL COUNTY TAX WITHHELD                    
         MVC   FRVLADD(L'FRVLADD),MYSPACES                                      
         MVI   FRVTFILE,C' '       BLANK                                        
         J     XIT                                                              
*                                                                               
         USING STACDD,RF                                                        
PRV70    MVC   FRVSCOD5,STACDE     STATE CODE                                   
         DROP  RF                                                               
         UNPK  FRVCNT5,TAPSCNT     NUMBER OF RS RECORDS                         
         OI    FRVCNT5+6,X'F0'                                                  
         MVC   FRVEIN5,EMPFDID     EMPLOYER ID NUMBER                           
         UNPK  FRVSWGS,STAWAGES    TOTAL STATE WAGES                            
         OI    FRVSWGS+14,X'F0'                                                 
         UNPK  FRVSTXWT,STATAXES   TOTAL STATE TAX WITHHELD                     
         OI    FRVSTXWT+14,X'F0'                                                
         MVC   FRVLWGS,ALLZERO4                                                 
         MVC   FRVLTXWT,ALLZERO4                                                
         J     XIT                                                              
*                                                                               
ALLZERO4 DC    C'000000000000000000000000000000000000000000000000' 48ZS         
         LTORG                                                                  
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        GET DATE AND TIME STAMP                                                
*        RETURNS THE TIMESTAMP AT 0(R6)                                         
***********************************************************************         
GETTMSTP NTR1  BASE=*,LABEL=*                                                   
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,TMPTIME                                                    
         ST    R1,FULL                                                          
         AP    TMPTIME(4),FULL                                                  
         CP    TMPTIME(4),=P'240000'                                            
         BL    *+10                                                             
         SP    TMPTIME(4),=P'240000'                                            
         ICM   R1,15,TMPTIME      ALIGN IT                                      
         SLL   R1,4                                                             
         STCM  R1,15,TMPTIME                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,TMPTHRS,0(R6),1                                      
         LA    R6,2(R6)                                                         
         GOTO1 HEXOUT,DMCB,TMPTMIN,0(R6),1                                      
         LA    R6,2(R6)                                                         
         GOTO1 HEXOUT,DMCB,TMPTSEC,0(R6),1                                      
         LA    R6,2(R6)                                                         
         MVC   0(2,R6),=CL2'00'     PUT ZEROS FOR NANOSECONDS                   
*                                                                               
         XIT1                                                                   
*                                                                               
TMPTIME  DS    0F                                                               
TMPTHRS  DS    X                                                                
TMPTMIN  DS    X                                                                
TMPTSEC  DS    X                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET TAREP2BNYA AT LEVEL 063 AS OF 03/20/00                      
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
         USING DLCBD,R3                                                         
INITDWN  NTR1  BASE=*,LABEL=*                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              TAPE SUPPORT                                           *         
***********************************************************************         
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTEQU,ACTDOWN                                                   
         BE    OPENT40                                                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   OPENTX                                                           
         L     R2,ASPFUSER         SAVE GENTAB IN USER AREA                     
         USING SPFUSERD,R2                                                      
*                                                                               
         LA    R1,GENTAB           R1=A(GENERATION TABLE)                       
         USING GENTBLD,R1                                                       
OPENT20  OC    GENEMP,GENEMP      ANY EMPLOYERS?                                
         BNZ   OPENT25                                                          
         MVC   GENEMP,TIFEMP      SAVE NEW EMPLOYER                             
         MVC   GENSTATE,DCBSTATE  AND STATE                                     
         B     OPENT30                                                          
OPENT25  CLC   GENEMP,TIFEMP      GENERATING NEW TAPE FOR SAME EMP?             
         BNE   *+14                                                             
         CLC   GENSTATE,DCBSTATE                  AND FOR SAME STATE?           
         BE    OPENT30                                                          
         LA    R1,GENTBLL(R1)                                                   
         B     OPENT20                                                          
OPENT30  ZIC   RF,GENNUM          BUMP GENERATION NUMBER FOR NEW TAPE           
         LA    RF,1(RF)                                                         
         STC   RF,GENNUM                                                        
         DROP  R1,R2                                                            
*                                                                               
         MVC   WORK(20),=CL20'TALTAPE.TA0WESS1'                                 
         MVC   WORK+12(1),TIFEMP          REPLACE 'E' WITH EMPLOYER             
         MVC   WORK+13(2),DCBSTATE        REPLACE 'SS' WITH STATE               
         MVC   DUB,=CL8'W2TAPE'                                                 
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DUB),((RF),WORK)                                
OPENT40  L     R2,ADCB                                                          
         OPEN  ((2),OUTPUT)                                                     
OPENTX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CLOSTAPE NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTEQU,ACTDOWN      DOWNLOADING                                  
         BE    CTAPE10                                                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   CTAPEX                                                           
CTAPE10  BRAS  RE,SPLAT                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         EDIT  (P6,TAPWCNT),(7,MYP)                                             
         MVC   MYP+8(17),=C'W RECORDS ON TAPE'                                  
         BRAS  RE,SPLAT                                                         
         EDIT  (P6,TAPOCNT),(7,MYP)                                             
         MVC   MYP+8(17),=C'O RECORDS ON TAPE'                                  
         BRAS  RE,SPLAT                                                         
         EDIT  (P6,TAPSCNT),(7,MYP)                                             
         MVC   MYP+8(17),=C'S RECORDS ON TAPE'                                  
         BRAS  RE,SPLAT                                                         
         DROP  R2                                                               
*                                                                               
         L     R2,ADCB                                                          
         CLOSE ((2))                                                            
CTAPEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
SPLAT    NTR1  BASE=*,LABEL=*                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BRAS  RE,MYCLEAR                                                       
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
MYCLEAR  NTR1  BASE=*,LABEL=*                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
EMUNCOMB NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTEQU,ACTDOWN                                                   
         BE    EMUNX                                                            
*                                                                               
         USING EMUNLSTD,R1                                                      
         LA    R1,EMUNLIST        CHECK EMP/UNIT COMBO                          
*                                                                               
EMUN10   CLC   WORK(5),EMEMPST                                                  
         BE    EMUNCCEQ                                                         
         LA    R1,EMUNLNQ(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   EMUN10                                                           
*                                                                               
EMUNCCNE LTR   RB,RB                                                            
         B     *+6                                                              
EMUNCCEQ CR    RB,RB                                                            
*                                                                               
EMUNX    XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
EMUNLIST DS    0D                  VALID EMP/UNIT COMBOS                        
*                                  CALL PHIL FOR DSN IF ADDING                  
         DC    C'PPAL '                                                         
         DC    C'PPAZ '                                                         
         DC    C'PPCA '                                                         
         DC    C'PPCT '                                                         
         DC    C'PPCO '                                                         
         DC    C'PPDC '                                                         
         DC    C'PPFD '                                                         
         DC    C'PPGA '                                                         
         DC    C'PPID '                                                         
         DC    C'PPIL '                                                         
         DC    C'PPKS '                                                         
         DC    C'PPKY '                                                         
         DC    C'PPLA '                                                         
         DC    C'PPMA '                                                         
         DC    C'PPMD '                                                         
         DC    C'PPMI '                                                         
         DC    C'PPMN '                                                         
         DC    C'PPMO '                                                         
         DC    C'PPMS '                                                         
         DC    C'PPMT '                                                         
         DC    C'PPNE '                                                         
         DC    C'PPNC '                                                         
         DC    C'PPNJ '                                                         
         DC    C'PPOR '                                                         
         DC    C'PPPA '                                                         
         DC    C'PPPR '                                                         
         DC    C'PPSC '                                                         
         DC    C'PPUT '                                                         
         DC    C'PPWA '                                                         
         DC    C'PPWI '                                                         
         DC    C'PPPHL'                                                         
*                                                                               
         DC    C'TPAL '                                                         
         DC    C'TPAR '            *** GOES TO CD, JAN 2010                     
         DC    C'TPAZ '            *** GOES TO CD, JAN 2010                     
         DC    C'TPCA '                                                         
         DC    C'TPCO '                                                         
         DC    C'TPCN '                                                         
         DC    C'TPCT '                                                         
         DC    C'TPDC '                                                         
         DC    C'TPDE '            *** GOES TO CD, JAN 2010                     
         DC    C'TPFD '                                                         
         DC    C'TPGA '                                                         
         DC    C'TPIL '            *** GOES TO CD, JAN 2010                     
         DC    C'TPIN '            *** GOES TO CD, JAN 2011                     
         DC    C'TPID '            *** GOES TO CD, JAN 2010                     
         DC    C'TPKY '            *** GOES TO CD, JAN 2003                     
         DC    C'TPKS '                                                         
         DC    C'TPLA '                                                         
         DC    C'TPMA '            *** GOES TO CD, JAN 2006                     
         DC    C'TPMD '                                                         
         DC    C'TPME '                                                         
         DC    C'TPMI '                                                         
         DC    C'TPMN '                                                         
         DC    C'TPMO '                                                         
         DC    C'TPMS '            *** GOES TO CD, JAN 2008                     
         DC    C'TPMT '                                                         
         DC    C'TPNC '                                                         
         DC    C'TPNE '            *** GOES TO CD, JAN 2011                     
         DC    C'TPNJ '            *** GOES TO CD, JAN 2010                     
         DC    C'TPNY '                                                         
         DC    C'TPPA '                                                         
         DC    C'TPPHL'                                                         
         DC    C'TPOR '                                                         
         DC    C'TPPR '                                                         
         DC    C'TPRI '            *** GOES TO CD, JAN 2010                     
         DC    C'TPSC '                                                         
         DC    C'TPUT '                                                         
         DC    C'TPVA '                                                         
         DC    C'TPVT '                                                         
         DC    C'TPWA '                                                         
         DC    C'TPWI '            *** GOES TO CD, JAN 2010                     
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* FIND STATE IN TABLE FOR ADDRESS LINE SWAP                                     
*                                                                               
SETADDR  NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,3(R1)          RECORD TYPE                                  
*                                                                               
         LA    RF,STATTAB                                                       
SADDR10  CLI   0(RF),X'FF'                                                      
         JE    NO                                                               
         CLC   W2STATE,0(RF)                                                    
         JE    *+12                                                             
         AHI   RF,STATTABQ                                                      
         J     SADDR10                                                          
*                                                                               
         CLI   BYTE,FRAQ           FRA RECORD?                                  
         JNE   SADDR20                                                          
         L     R2,=A(FRA)                                                       
         USING FRA,R2                                                           
         MVC   FRACLADD,TADCADD    LOCATION ADDRESS                             
         MVC   FRACDADD,TADCADD2   DELIVERY ADDRESS                             
         CLI   3(RF),YQ            SWAP ADDRESS LINES?                          
         JNE   NO                                                               
*        MVC   FRACLADD,TADCADD2   LOCATION ADDRESS                             
         MVC   FRACLADD,=CL(L'FRACLADD)'SUITE 1525'                             
         MVC   FRACDADD,TADCADD    DELIVERY ADDRESS                             
         J     YES                                                              
*                                                                               
SADDR20  CLI   BYTE,FREQ           FRE RECORD?                                  
         JNE   SADDR30                                                          
         L     R2,=A(FRE)                                                       
         USING FRE,R2                                                           
         MVC   FRELADD,TADCADD     LOCATION ADDRESS                             
         MVC   FREDADD,TADCADD2    DELIVERY ADDRESS                             
         OC    FRELADD,MYSPACES                                                 
         OC    FREDADD,MYSPACES                                                 
         CLI   4(RF),YQ            SWAP ADDRESS LINES?                          
         JNE   NO                                                               
*        MVC   FRELADD,TADCADD2     LOCATION ADDRESS                            
         MVC   FRELADD,=CL(L'FRELADD)'SUITE 1525'                               
         MVC   FREDADD,TADCADD      DELIVERY ADDRESS                            
         OC    FRELADD,MYSPACES                                                 
         OC    FREDADD,MYSPACES                                                 
         J     YES                                                              
*                                                                               
SADDR30  CLI   BYTE,FRSQ           FRS RECORD?                                  
         JNE   SADDR40                                                          
         L     R2,=A(FRS)                                                       
         USING FRS,R2                                                           
         MVC   FRSLADD,TADCADD     LOCATION ADDRESS                             
         MVC   FRSDADD,TADCADD2    DELIVERY ADDRESS                             
         OC    FRSLADD,MYSPACES                                                 
         OC    FRSDADD,MYSPACES                                                 
         CLI   5(RF),YQ            SWAP ADDRESS LINES?                          
         JNE   NO                                                               
         MVC   FRSLADD,TADCADD2    LOCATION ADDRESS                             
         MVC   FRSDADD,TADCADD     DELIVERY ADDRESS                             
         OC    FRSLADD,MYSPACES                                                 
         OC    FRSDADD,MYSPACES                                                 
         J     YES                                                              
*                                                                               
SADDR40  CLI   BYTE,FRTQ           FRT RECORD?                                  
         JNE   SADDR50                                                          
         L     R2,=A(FRT)                                                       
         USING FRT,R2                                                           
         CLI   6(RF),YQ            SWAP ADDRESS LINES?                          
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
SADDR50  CLI   BYTE,FRWQ           FRW RECORD?                                  
         JNE   NO                                                               
         L     R2,=A(FRW)                                                       
         USING FRW,R2                                                           
         MVC   FRWLADD,TADCADD     LOCATION ADDRESS                             
         MVC   FRWDADD,TADCADD2    DELIVERY ADDRESS                             
         OC    FRWLADD,MYSPACES                                                 
         OC    FRWDADD,MYSPACES                                                 
         CLI   7(RF),YQ            SWAP ADDRESS LINES?                          
         JNE   NO                                                               
         MVC   FRWLADD,TADCADD2    LOCATION ADDRESS                             
         MVC   FRWDADD,TADCADD     DELIVERY ADDRESS                             
         OC    FRWLADD,MYSPACES                                                 
         OC    FRWDADD,MYSPACES                                                 
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
FRAQ     EQU   C'A'                                                             
FREQ     EQU   C'E'                                                             
FRSQ     EQU   C'S'                                                             
FRTQ     EQU   C'T'                                                             
FRWQ     EQU   C'W'                                                             
YQ       EQU   X'80'               YES                                          
NQ       EQU   X'40'               NO                                           
*                                                                               
* SEP 2015 - CHANGES TO SWAP ADDRESS LINE 1 & 2 FROM W4 RECORD                  
*                                                                               
* TABLE ENTRY = STATE,FRA,FRE,FRS,FRT,FRW                                       
*       AL1(YQ) = SWAP ADDRESS LINES                                            
*       AL1(NQ) = KEEP ADDRESS LINES THE SAME                                   
*                                                                               
*                                                                               
STATTAB  DC    C'AL ',AL1(NQ),AL1(NQ),AL1(NQ),AL1(NQ),AL1(NQ)                   
STATTABQ EQU   *-STATTAB                                                        
         DC    C'AR ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'AZ ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'CO ',AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ),AL1(NQ)                   
         DC    C'CT ',AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ),AL1(NQ)                   
         DC    C'DC ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'DE ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'FD ',AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ),AL1(YQ)                   
         DC    C'GA ',AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ),AL1(YQ)                   
         DC    C'ID ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'IL ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'IN ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'KS ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'KY ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'LA ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'ME ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'MA ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'MD ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'MI ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'MN ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'MO ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'MS ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'MT ',AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ),AL1(YQ)                   
         DC    C'NC ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'NE ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'NJ ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'NM ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'OH ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'OK ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'OR ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'PA ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'PHL',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(YQ)                   
         DC    C'PR ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'RI ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'SC ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'UT ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'VT ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'VA ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    C'WI ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ),AL1(YQ)                   
         DC    C'WV ',AL1(YQ),AL1(YQ),AL1(YQ),AL1(NQ),AL1(NQ)                   
         DC    X'FF'                                                            
*                                                                               
* TASYSEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
* TASYSDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              MY WORKING STORAGE                                               
*                                                                               
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
       ++INCLUDE TAADDCOND                                                      
MYREGS   DS    16F                                                              
MYASS    DS    A                                                                
MYTITLE  DS    CL32                                                             
*                                                                               
MYP      DS    0CL132                                                           
MYP1     DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYP5     DS    CL132                                                            
MYP6     DS    CL132                                                            
MYP7     DS    CL132                                                            
*                                                                               
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
SKIPSW   DS    XL1                                                              
FEDPRSW  DS    CL1                                                              
TAPETYPE DS    XL1                 FEDERAL OR STATE SET                         
FEDERAL  EQU   0                                                                
NEWYORK  EQU   1                                                                
ILLINOIS EQU   2                                                                
CALIF    EQU   3                                                                
LOUISANA EQU   4                                                                
GENERIC  EQU   4                                                                
RHODEI   EQU   5                                                                
CONN     EQU   6                                                                
DCBSTATE DS    CL3                                                              
W2STATE  DS    CL3                                                              
GPVWRK   DS    C                                                                
GPVSTATE DS    CL2                                                              
PRVSTATE DS    CL2                                                              
SRTCOUNT DS    PL4                 COUNT OF SORT RECS                           
ADCB     DS    A                                                                
DYNALLOC DS    A                                                                
ASPFUSER DS    A                                                                
AMASTD   DS    A                                                                
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
*                                                                               
LASTSSN  DS    CL9                                                              
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 TRACE=NNNN                                   
TRALIMIT DS    PL6                                                              
*                                                                               
RECLIMIT DS    PL6                 LIMIT=NNNN                                   
*                                                                               
REPLIMIT DS    PL6                 REPORT=NNNN                                  
*                                                                               
PRINTOPT DS    CL1                 PRINT=Y PRINT W2 FORMS                       
*                                                                               
TAPEOPT  DS    CL1                 TAPE=Y WRITE W2 TAPE                         
*                                                                               
SUBSOPT  DS    CL1                 SUBS=Y HANDLE SUB RECORDS                    
*                                                                               
CHECKOPT DS    CL1                 CHECKADD=Y CHECK ADDRESSES                   
*                                                                               
DIAGOPT  DS    CL1                 DIAG=Y PRINT DIAGNOSTICS                     
*                                                                               
CORPOPT  DS    CL1                 CORP=Y INCLUDE CORPS & FOREIGN               
*                                                                               
RESUBOPT DS    CL1                 RESUB=TLCN                                   
QTROPT   DS    XL1                 QUARTER NUMBER, S RECORDS ONLY               
GPVOPT   DS    XL1                 GUAM, PUERTO RICO, VI ONLY                   
TLCN     DS    CL6                 TLCN NUMBER                                  
*                                                                               
CTRLNUM  DS    PL6                 CONTROL NUMBER START                         
*                                                                               
*                                                                               
NEWW4AD  DS    CL1                 NEW W4 ADDRESS FOUND                         
*                                                                               
RECTYPE  DS    CL16                                                             
PTODAY   DS    PL3                                                              
*                                                                               
ATHISEX  DS    A                                                                
EMPNAME  DS    CL36                                                             
EMPADD   DS    CL120                                                            
EMPFDID  DS    CL14                                                             
ATHISW4  DS    A                                                                
W4NAME   DS    CL36                                                             
W4MIDN   DS    CL15                                                             
W4SUFF   DS    CL4                                                              
W4ADD    DS    CL120                                                            
SAVEEL   DS    XL1                                                              
W4STA2   DS    XL1                                                              
W4TYPE   DS    XL1                                                              
THISUNIT DS    CL3                                                              
STATUNIT DS    CL3                                                              
SSNSTART DS    CL9                                                              
THISSSN  DS    CL9                                                              
UNITLIST DS    CL240                                                            
         DS    0D                                                               
FEDACCS  DS    CL(ACCLNQ)          PERFORNER ACCUMULATORS                       
EMPACCS  DS    CL(ACCLNQ)          EMPLOYER      "                              
FINACCS  DS    CL(ACCLNQ)          REQUEST       "                              
NLEVELS  EQU   (*-FEDACCS)/ACCLNQ                                               
*                                                                               
CPRWAGES DS    PL8                 PUERTO RICO WAGES                            
CPRTAXES DS    PL8                 PUERTO RICO TAXES                            
CPRFWAGE DS    PL8                 PUERTO RICO FICA WAGES                       
CPRFTAX  DS    PL8                 PUERTO RICO FICA TAXES                       
CPRMWAGE DS    PL8                 PUERTO RICO MEDICARE WAGES                   
CPRMTAX  DS    PL8                 PUERTO RICO MEDICARE TAXES                   
         DS    CL16                CUSHION                                      
*                                                                               
STACODE  DS    CL2                                                              
STANAME  DS    CL8                                                              
         DS    0D                                                               
*                                                                               
YEAR     DS    CL4                                                              
*                                                                               
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDMIDN DS    CL15                                                             
NEEDSUFF DS    CL4                                                              
NEEDADD  DS    CL120                                                            
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                 ** NO LONGER USED                            
NEEDSTA2 DS    CL1                 ** NO LONGER USED                            
*                                                                               
TIDUNIT  DS    CL3                                                              
TIDID    DS    CL14                                                             
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
QUO      DS    XL1                                                              
REM      DS    XL1                                                              
         DS    CL8                 CUSHION                                      
W2NHAWG  DS    PL8                 W2 NEW HIRE ACT EXEMPT WAGE                  
*                                                                               
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER ACCUMULATOR ENTRIES                               
*                                                                               
ACCUMD   DSECT                                                                  
ACCS     DS    0PL8                                                             
FEDTAXES DS    PL8                 TAX AMOUNT                                   
FEDWAGES DS    PL8                 WAGES                                        
FEDREXP  DS    PL8                 REIMBURSED EXPENSES                          
FICTAXES DS    PL8                 FICA TAX AMOUNT                              
FICWAGES DS    PL8                 FICA WAGES                                   
MEDTAXES DS    PL8                 MEDICARE TAX AMOUNT                          
MEDWAGES DS    PL8                 MEDICARE WAGES                               
STATAXES DS    PL8                 STATE TAX AMOUNT                             
STAWAGES DS    PL8                 STATE WAGES                                  
STASDI   DS    PL8                 STATE SDI                                    
STASUI   DS    PL8                 STATE SUI                                    
LOCTAXES DS    PL8                 LOCAL TAX AMOUNT                             
LOCWAGES DS    PL8                 LOCAL WAGES                                  
NHAWAGES DS    PL8                 NHA EXEMPT WAGES                             
*                                                                               
TPRWAGES DS    PL8                 PUERTO RICO WAGES                            
TPRTAXES DS    PL8                 PUERTO RICO TAXES                            
TPRFWAGE DS    PL8                 PUERTO RICO FICA WAGES                       
TPRFTAX  DS    PL8                 PUERTO RICO FICA TAXES                       
TPRMWAGE DS    PL8                 PUERTO RICO MEDICARE WAGES                   
TPRMTAX  DS    PL8                 PUERTO RICO MEDICARE TAXES                   
TPRNHAWG DS    PL8                 PUERTO RICO NHA EXEMPT WAGES                 
ACCNEXT  EQU   *                                                                
ACCLNQ   EQU   *-ACCUMD                                                         
NACCS    EQU   ACCLNQ/L'ACCS                                                    
         EJECT                                                                  
*              DSECT TO COVER STATE/CITY CODE TABLE                             
*                                                                               
STACDD   DSECT                                                                  
STASTA   DS    CL3                 STATE/CITY                                   
STACDE   DS    CL2                 STATE CODE                                   
STACLNQ  EQU   *-STACDD                                                         
         EJECT                                                                  
*              DSECT TO COVER EMPLOYER STATE/CITY LIST                          
*                                                                               
EMUNLSTD DSECT                                                                  
EMEMPST  DS    CL5                 EMPLOYER/STATE                               
EMUNLNQ  EQU   *-EMUNLSTD                                                       
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRINTD   DSECT                                                                  
PRSTART  DC    CL01' '                                                          
PRSSN    DC    CL09' '                                                          
         DC    CL01' '                                                          
PRNAME   DC    CL29' '                                                          
         DC    CL01' '                                                          
PRWAGES  DC    CL12' '                                                          
         DC    CL01' '                                                          
PRTAXES  DC    CL12' '                                                          
         DC    CL01' '                                                          
PRSTAWAG DS    0CL12                                                            
PRFICWAG DC    CL12' '                                                          
         DC    CL01' '                                                          
PRSTATAX DS    0CL12                                                            
PRFICA   DC    CL12' '                                                          
         DC    CL01' '                                                          
PRSDI    DS    0CL12                                                            
PRMEDWAG DC    CL12' '                                                          
         DC    CL01' '                                                          
PRLOCWAG DS    0CL12                                                            
PRSUI    DS    0CL12                                                            
PRMEDCAR DC    CL12' '                                                          
         DC    CL01' '                                                          
PRLOCTAX DS    0CL12                                                            
PRFB     DC    CL12' '                                                          
         DC    CL01' '                                                          
*                                                                               
         ORG   PRSTART                                                          
         DC    CL01' '                                                          
PCKNAME  DC    CL33' '                                                          
         DC    CL01' '                                                          
PCKADD   DC    CL30' '                                                          
         DC    CL01' '                                                          
PCKSTR   DC    CL40' '                                                          
         DC    CL01' '                                                          
PCKDIAG  DC    CL24' '                                                          
         DC    CL01' '                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
*                                                                               
MYPRINT  DSECT                                                                  
         DS    CL132                                                            
*                                                                               
*              DSECT TO COVER TSPFUSER                                          
*                                                                               
SPFUSERD DSECT                                                                  
ABUFFER  DS    A                  EMPLOYER                                      
GENTAB   DS    255CL(GENTBLL)     GENERATION TABLE                              
*                                                                               
*              DSECT TO COVER GENTAB (IN TSPFUSER)                              
*                                                                               
GENTBLD  DSECT                                                                  
GENEMP   DS    CL1                EMPLOYER                                      
GENSTATE DS    CL2                STATE                                         
GENNUM   DS    XL1                LATEST GENERATION NUM. FOR EMP/STATE          
GENTBLL  EQU   *-GENTBLD                                                        
         EJECT                                                                  
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDLOGOD                                                        
         DSECT                                                                  
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDTWADCONS                                                                     
         PRINT OFF                                                              
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPCFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064TAREP2F   01/24/17'                                      
         END                                                                    
