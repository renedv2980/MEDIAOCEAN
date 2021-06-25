*          DATA SET DDPAPTMRR  AT LEVEL 015 AS OF 10/07/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTMRRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DDINFO                                                                 
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'PANAPT: PERFORM PRE-MODELING PROCESSES'                         
*                                                                               
*======================================================================         
*                                                                               
* THIS MODULE IS EXECUTED BY THE PAPTSTC STARTED TASK. IT RECEIVES A            
* SPACE-DELIMITED "PARM=" STRING (AVAILABLE VIA R1).                            
*                                                                               
* THE OVERALL PURPOSE OF THIS MODULE IS TO CREATE TEMPORARY DATASETS            
* AND GLOBAL PANAPT VARIABLES WHICH ARE NEEDED DURING PANAPT'S                  
* MODELING PROCESS.                                                             
*                                                                               
*======================================================================         
*                                                                               
PAPTMRR  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PAPTMRR,=V(REGSAVE),R9                                         
*                                                                               
* SPACE-DELIMITED PARAMETER STRING IS CONSTRUCTED BY DDJIPAPT                   
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)          L'PARM STRING                                
         JNP   *+2                 NO PARMS ?!?                                 
*                                                                               
         LA    R1,2(R1)            BUMP PAST PARM LENGTH                        
         LA    RE,SUBMITTR         FIRST PARM MUST BE MR SUBMITTER              
GETUSRID MVC   0(1,RE),0(R1)       PARAMETER IS DELIMITED BY A BLANK            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         SHI   RF,1                                                             
         BZ    MAIN05              DON'T LOOK BEYOND PARM LENGTH                
         CLI   0(R1),C' '          END OF SUBMITTER USERID?                     
         BNE   GETUSRID            NO                                           
*                                                                               
         MVC   ENVIRON,1(R1)       NEXT 3 CHARS MUST BE ENV. PREFIX             
         CLC   ENVIRON,=C'DDS'                                                  
         BE    MAIN05                                                           
         CLC   ENVIRON,=C'SBX'                                                  
         BE    MAIN05                                                           
         DC    H'0'                UNKNOWN ENVIRONMENT ?!?                      
*                                                                               
MAIN05   DS    0H                                                               
         SHI   RF,5                LENGTH OF " DDS " OR " SBX "                 
         LA    RE,ITMFWHO                                                       
         LA    R1,5(R1)            POINT TO ITMF SUBMITTER                      
BLDITMF  MVC   0(1,RE),0(R1)       PARAMETER IS DELIMITED BY A BLANK            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         SHI   RF,1                                                             
         BZ    MAIN07              DON'T LOOK BEYOND PARM LENGTH                
         CLI   0(R1),C' '          END OF ITMF SUBMITTER'S USERID?              
         BNE   BLDITMF             NO                                           
*                                                                               
MAIN07   DS    0H                                                               
         SHI   RF,1                LENGTH OF BLANK DELIMITER                    
         BM    MAIN09              4TH PARAMETER IS NOT PRESENT                 
         LA    RE,ITMFTKT#                                                      
         LA    R1,1(R1)            POINT TO ITMF ISSUE #                        
BLDTKT#  MVC   0(1,RE),0(R1)       PARAMETER IS DELIMITED BY A BLANK            
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         SHI   RF,1                                                             
         BZ    MAIN09              DON'T LOOK BEYOND PARM LENGTH                
         CLI   0(R1),C' '          END OF ITMF SUBMITTER'S USERID?              
         BNE   BLDTKT#             NO                                           
*                                                                               
MAIN09   DS    0H                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(PAPTMRR),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
MAIN10   DS    0H                                                               
         MVC   TITLE,=CL60'PANAPT - PERFORM PRE-MODELING PROCESSES'             
*                                                                               
         MVC   P(24),=C'MOVE REQUEST SUBMITTER: '                               
         MVC   P+24(8),SUBMITTR     MR SUBMITTER                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'ENVIRONMENT: '                                          
         MVC   P+13(3),ENVIRON      ENVIRONMENT PREFIX ('DDS' OR 'SBX')         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'ITMF TICKET SUBMITTER: '                                
         MVC   P+23(8),ITMFWHO      ITMF TICKET SUBMITTER'S TSO USERID          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(24),=C'ITMF JIRA ISSUE #: ITMF-'                               
         MVC   P+24(11),ITMFTKT#    ITMF JIRA ISSUE #                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    RE,ITMFVAR+23        BUILD ITMF SUBMITTER VARIABLE               
         LA    RF,ITMFWHO                                                       
MAIN20   DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BE    MAIN30                                                           
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     MAIN20                                                           
MAIN30   DS    0H                                                               
         MVI   0(RE),C''''                                                      
*                                                                               
         LA    RE,ITMFISS#+26       BUILD ITMF JIRA ISSUE # VARIABLE            
         LA    RF,ITMFTKT#                                                      
MAIN40   DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BE    MAIN50                                                           
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     MAIN40                                                           
MAIN50   DS    0H                                                               
         MVI   0(RE),C''''                                                      
*                                                                               
         THMS  ,                                                                
         ST    R1,FULL             R1=0HHMMSS+                                  
         OI    FULL+3,X'0F'                                                     
         UNPK  CURRTIME+22(6),FULL C'HHMMSS'                                    
*                                                                               
* EXTRACT THE VOLSER FOR EACH LOAD LIBRARY THAT MIGHT BE INVOLVED IN A          
* MOVE REQUEST. IEHPROGM DEMANDS THAT A DD STATEMENT BE PRESENT WITH            
* ALL VOLSERS NAMED. THE APPROACH BELOW IS SOFT, SO THAT IF OPS MOVES           
* A LOAD LIBRARY TO A NEW VOLSER, IT WON'T DESTROY THE LOAD MODULE              
* MANIPULATION STEP(S) GENERATED BY PANAPT.                                     
*                                                                               
         LA    R2,LOADLIB_TABLE                                                 
         USING LOADLIB_TABLED,R2                                                
NEXTDD   DS    0H                                                               
         MVC   WORK,LOADLIB_DSN    ASSUME NOT SANDBOX                           
         CLC   ENVIRON,=C'SBX'     ARE WE IN THE SANDBOX?                       
         BNE   *+16                NO: BYPASS ADJUSTMENT                        
         MVC   WORK(L'SANDBOX),SANDBOX                                          
         MVC   WORK+L'SANDBOX(L'SANDBOX),LOADLIB_DSN                            
         MVC   LOADLIB_DSN,WORK                                                 
*                                                                               
* DYNAMICALLY ALLOCATE THE LOAD LIBRARY WITH DISP=SHR (IN CASE IT               
* ISN'T ALLOCATED ALREADY). THIS IS A PREREQUISITE FOR THE DYNAMIC              
* INFORMATION RETRIEVAL THAT FOLLOWS.                                           
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'FF',LOADLIB_DDNAME),LOADLIB_DSN             
*                                                                               
         GOTO1 =V(DDINFO),DMCB,(LOADLIB_DDNAME_LEN,LOADLIB_DDNAME),    +        
               =AL2(DINRTVOL)                                                   
         CLI   DMCB+8,6            RETURNED L'VOLSER                            
         BE    *+6                                                              
         DC    H'0'                I ALWAYS EXPECT VOLSER LENGTH OF 6 !         
         L     RE,DMCB+8                                                        
         MVC   LOADLIB_VOLSER,0(RE)  RETURNED VOLSER                            
*                                                                               
         MVC   P_LOADLIB_DDNAME,LOADLIB_DDNAME                                  
         MVC   P_LOADLIB_DSN,LOADLIB_DSN                                        
         MVC   P_LOADLIB_VOLSER,LOADLIB_VOLSER                                  
         GOTO1 =V(PRINTER)         PRINT A TABLE OF HELPFUL INFO                
*                                                                               
         LA    R2,LOADLIB_TABLE_LEN(R2)   BUMP TO NEXT LOAD LIBRARY             
         CLI   0(R2),X'FF'         EOT?                                         
         BNE   NEXTDD                                                           
         DROP  R2                                                               
*                                                                               
* THIS IS HARD CODE WHICH ASSUMES THAT THERE ARE EXACTLY TWO ENTRIES            
* IN THE LOADLIB TABLE. SEE THE LOADLIB_TABLE COMMENT BELOW.                    
*                                                                               
LLIB     USING LOADLIB_TABLED,LOADLIB                                           
TLIB     USING LOADLIB_TABLED,TESTLIB                                           
*                                                                               
         LA    RE,LOADLIB_DATASETNAME+20                                        
         LA    RF,LLIB.LOADLIB_DSN BUILD LOADLIB DSN VARIABLE                   
LDDSN10  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BE    LDDSN20                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     LDDSN10                                                          
LDDSN20  DS    0H                                                               
         MVI   0(RE),C''''                                                      
*                                                                               
         LA    RE,TESTLIB_DATASETNAME+20                                        
         LA    RF,TLIB.LOADLIB_DSN BUILD TESTLIB DSN VARIABLE                   
TSTDSN10 DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BE    TSTDSN20                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     TSTDSN10                                                         
TSTDSN20 DS    0H                                                               
         MVI   0(RE),C''''                                                      
*                                                                               
         MVC   LOADLIB_VOLSER#+23(6),LLIB.LOADLIB_VOLSER                        
         MVC   TESTLIB_VOLSER#+23(6),TLIB.LOADLIB_VOLSER                        
*                                                                               
         CLC   LLIB.LOADLIB_VOLSER,TLIB.LOADLIB_VOLSER  SAME LOADLIB?           
         BNE   *+14                                                             
         MVC   VOLSERS+21(6),LLIB.LOADLIB_VOLSER  YES: SYNTAX IS EASY           
         B     FINDVER                                                          
*                                                                               
* CONSTRUCT THE STRING TO BE ASSIGNED TO THE SOFT PANAPT VARIABLE.              
*                                                                               
         MVI   VOLSERS+21,C'('     BUILD VOLSER LIST                            
         MVC   VOLSERS+22(6),LLIB.LOADLIB_VOLSER                                
         MVI   VOLSERS+28,C','     DELIMIT EACH VOLSER WITH A COMMA             
         MVC   VOLSERS+29(6),TLIB.LOADLIB_VOLSER                                
         MVI   VOLSERS+35,C')'     END THE LIST                                 
         MVI   VOLSERS+36,C''''    END THE PANAPT VARIABLE EXPRESSION           
*                                                                               
         DROP  LLIB,TLIB                                                        
         EJECT                                                                  
FINDVER  DS    0H                                                               
         L     R1,FLCCVT-PSA(,0)   A(CVT)                                       
         LA    R1,CVTSNAME-CVT(,R1)   SYSTEM ID (E.G., 'SY7     ')              
         MVC   SYSTEMID+17(3),0(R1)   SET THE $G$SYSTEMID VARIABLE              
*                                                                               
         MVC   FIRSTRVP,=14X'FF'   INIT DATE/TIME TO HIGH VALUES                
*                                                                               
         OPEN  MRFILE                                                           
*                                                                               
         GET   MRFILE,APAMMDES_RDW                                              
         CLC   DESTYPE,=C'01'      FIRST RECORD MUST BE "01"                    
         BE    *+6                                                              
         DC    H'0'                SOMETHING IS TERRIBLY WRONG WITH MR          
*                                                                               
         MVC   P(132),DESTYPE      PRINT '01' RECORD                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* IF THIS IS A COPY FOR REWORK, THE CHANGE NAME FIELD WILL LOOK LIKE:           
*   'RW OOOOOO:RRRRRR' (OOOOOO = ORIGINAL MR#, RRRRRR = REWORK MR#)             
         CLC   =C'RW ',DESNAME     IS THIS A COPY FOR REWORK MR?                
         BNE   NEXTVER                                                          
         CLI   DESNAME_DELIMITER,C':'                                           
         BNE   NEXTVER                                                          
         MVI   REWRKFLG+15,C'Y'    YES: SET VALUE IN GLOBAL VARIABLE            
         MVC   ORIGMR#+21(6),DESNAME_ORIG_MR#  SAVE ORIG. MR# IN GLOBAL         
*                                                                               
NEXTVER  DS    0H                                                               
         GET   MRFILE,APAMMVER_RDW                                              
         CLC   MRVRECTP,=C'04'     LOOK FOR A VERIFICATION RECORD               
         BNE   NEXTVER                                                          
*                                                                               
         MVC   P(132),MRVRECTP     PRINT ALL '04' RECS                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   DESCLSN,MRVKVLVL    FIND EARLIEST 04 REC FOR CURRENT LVL         
         BNE   NEXTVER                                                          
*                                                                               
         CLC   FIRSTRVP(L'MRVDATE+L'MRVTIME),MRVDATE                            
         BL    *+10                                                             
         MVC   FIRSTRVP(L'MRVDATE+L'MRVTIME),MRVDATE                            
         B     NEXTVER                                                          
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE MRFILE                                                           
*                                                                               
* RVPFILE CONTAINS A SINGLE CL80 RECORD OF THE FOLLOWING FORMAT:                
*  CL14: EARLIEST VERIFICATION DATE/TIME IN YYYYMMDDHHMMSS FORMAT               
*         (OR HIGH VALUES) FOR THE CURRENT MR LEVEL                             
*  CL8:  JOB SUBMITTER'S TSO USERID                                             
*  CL8:  ITMF TICKET SUBMITTER'S TSO USERID                                     
*  CL16  ITMF JIRA ISSUE #                                                      
*  CL34: SPARE (SPACES)                                                         
*                                                                               
* THIS FILE IS READ BY DDPAPTMEX (THE MEMBER EXISTENCE EXIT) TO ENSURE          
* THAT NO PANVALET SOURCE MEMBERS IN A MOVE REQUEST ARE MODIFIED AFTER          
* THE EARLIEST VERIFICATION PROCEDURE RUNS FOR THE CURRENT LEVEL.               
*                                                                               
* IF NO ELIGIBLE VERIFICATION RECORD EXISTS FOR THE MR, HIGH VALUES ARE         
* WRITTEN SO THAT DDPAPTMEX DOESN'T COMPLAIN.                                   
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(14),FIRSTRVP   EARLIEST VERIFICATION DATE/TIME              
         MVC   WORK+14(8),SUBMITTR SUBMITTER'S TSO USERID (FROM PARM=)          
         MVC   WORK+22(8),ITMFWHO  ITMF TICKET SUBMITTER'S USERID               
         MVC   WORK+30(5),=C'ITMF-' JIRA PROJECT IS ALWAYS "ITMF"               
         MVC   WORK+35(11),ITMFTKT# ITMF JIRA ISSUE #                           
*                                                                               
         OPEN  (RVPFILE,OUTPUT)                                                 
         PUT   RVPFILE,WORK                                                     
         CLOSE RVPFILE                                                          
*                                                                               
         OPEN  (GLOBALS,OUTPUT)                                                 
         PUT   GLOBALS,REWRKFLG            $G$REWORK                            
         PUT   GLOBALS,ORIGMR#             $G$ORIGINAL_MR#                      
         PUT   GLOBALS,VOLSERS             $G$SYSDD_DDSVOL                      
         PUT   GLOBALS,LOADLIB_VOLSER#     $G$LOADLIB_VOLSER                    
         PUT   GLOBALS,TESTLIB_VOLSER#     $G$TESTLIB_VOLSER                    
         PUT   GLOBALS,LOADLIB_DATASETNAME $G$LOADLIB_DSN                       
         PUT   GLOBALS,TESTLIB_DATASETNAME $G$TESTLIB_DSN                       
         PUT   GLOBALS,SYSTEMID            $G$SYSTEMID                          
         PUT   GLOBALS,ITMFVAR             $G$ITMF_SUBMITTER                    
         PUT   GLOBALS,ITMFISS#            $G$ITMF_ISSUE_#                      
         PUT   GLOBALS,CURRTIME            $G$MODELING_TIME                     
         CLOSE GLOBALS                                                          
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
FIRSTRVP DS    CL14                YYYYMMDDHHMMSS                               
WORK     DS    CL80                                                             
SUBMITTR DC    CL8' '              JOB SUBMITTER                                
ITMFWHO  DC    CL8' ',C' '         ITMF TICKET SUBMITTER                        
ITMFTKT# DC    CL11'0',C' '        ITMF JIRA ISSUE NUMBER                       
ENVIRON  DS    CL3                 'DDS' OR 'SBX'                               
         EJECT                                                                  
*======================================================================         
*                                                                               
*                       PANAPT GLOBAL VARIABLES                                 
*                                                                               
* THERE IS INFORMATION THAT WE CAN DERIVE PROGRAMMATICALLY IN THIS              
* PROGRAM WHICH WE WANT TO MAKE AVAILABLE TO PANAPT DURING MODELING.            
* WE DO THIS BY GENERATING MODEL STATEMENTS TO DEFINE GLOBAL PANAPT             
* VARIABLES. IN THIS WAY, WE HAVE ACCESS TO CRITICAL INFORMATION DURING         
* MODELING THAT ISN'T AVAILABLE VIA PREDEFINED PANAPT KEYWORDS.                 
*                                                                               
* THE MODEL STATEMENTS ARE WRITTEN TO A TEMPORARY DATASET BY THIS               
* PROGRAM, AND ARE SUBSEQUENTLY CONCATENATED TO PANAPT'S "APTMODEL" DD.         
*                                                                               
*======================================================================         
*                                                                               
*                                                                               
* INDICATE WHETHER OR NOT THIS MOVE REQUEST IS A COPY FOR REWORK. THE           
* REWORK DETAILS ARE ONLY AVAILABLE IN THE "CHANGE NAME" (DESNAME)              
* FIELD IN APAMMDES. IT IS PUT THERE BY OUR CUSTOMIZED APIP110 PANEL.           
* THE CHANGE NAME IS NOT AVAILABLE AS A MODELING KEYWORD, HENCE THE             
* NEED TO SAVE THE REWORK FLAG HERE AS A GLOBAL VARIABLE.                       
* ALSO GENERATE A VARIABLE CONTAINING THE ORIGINAL MR# FOR THIS REWORK.         
*                                                                               
REWRKFLG DC    CL80'@ $G$REWORK = ''N'''   ASSUME NOT A COPY FOR REWORK         
ORIGMR#  DC    CL80'@ $G$ORIGINAL_MR# = ''      '''    ORIGINAL MR #            
*                                                                               
* IEHPROGM REQUIRES A DD STATEMENT LISTING ALL VOLSERS BEING UPDATED.           
* BY GENERATING THIS MODEL STATEMENT HERE, WE DON'T NEED TO HARD-CODE           
* THE VOLSER(S) IN THE DD STATEMENTS. (SO IF A LOAD LIBRARY IS MOVED            
* TO A NEW VOLSER, IT WON'T RESULT IN A JCL ERROR.)                             
*                                                                               
* WE HAVE ALSO ADDED INDIVIDUAL DSN AND VOLSER VARIABLES FOR EACH LOAD          
* LIBRARY. NOTE THAT WE ASSUME THAT THERE ARE EXACTLY TWO LOAD                  
* LIBRARIES TO CONSIDER (SEE LOADLIB_TABLE COMMENT BELOW).                      
*                                                                               
VOLSERS  DC    CL80'@ $G$SYSDD_DDSVOL = ''XXXXXX'''                             
*                                                                               
LOADLIB_VOLSER#     DC CL80'@ $G$LOADLIB_VOLSER = ''XXXXXX'''                   
TESTLIB_VOLSER#     DC CL80'@ $G$TESTLIB_VOLSER = ''XXXXXX'''                   
LOADLIB_DATASETNAME DC CL80'@ $G$LOADLIB_DSN = '''                              
TESTLIB_DATASETNAME DC CL80'@ $G$TESTLIB_DSN = '''                              
*                                                                               
* THIS GLOBAL VARIABLE ALLOWS THE PANAPT MODELS TO REACT TO THE LPAR            
* ON WHICH THE MOVE JOBS ARE RUNNING. (FOR EXAMPLE, IF SY7                      
* IS UNAVAILABLE, WE MIGHT RUN PANAPT ON SY1. IN THAT CASE DON'T WANT           
* TO GENERATE THE PAPTWAIT AND PAPTDONE STEPS.)                                 
*                                                                               
SYSTEMID DC    CL80'@ $G$SYSTEMID = ''XXX'''                                    
*                                                                               
* THESE VARIABLES ARE DERIVED FROM FIELDS ENTERED BY THE OPERATOR ON            
* THE APIP200 (MR "SUB") PANEL.                                                 
* WE NEED THE TSO USERID OF THE ITMF JIRA TICKET CREATOR (I.E., THE             
* DEVELOPER), SO THAT WE CAN CC THE DEVELOPER ON ANY AUTO-GENERATED             
* E-MAILS.                                                                      
* WE NEED A VARIABLE FOR THE ITMF JIRA ISSUE NUMBER, SO THAT WHEN THE           
* "SUB" PROCESS IS COMPLETE, WE CAN PROGRAMMATICALLY ADD AN ATTACHMENT          
* TO THE ITMF TICKET VIA THE JIRA API (FOR AUDITING PURPOSES).                  
*                                                                               
ITMFVAR  DC    CL80'@ $G$ITMF_SUBMITTER = '''                                   
ITMFISS# DC    CL80'@ $G$ITMF_ISSUE_# = ''ITMF-'                                
*                                                                               
* PANAPT HAS A KEYWORD CALLED $TODAY WHICH IS THE CURRENT DATE IN               
* YYYYMMDD FORMAT, BUT IT HAS NO KEYWORD FOR THE CURRENT *TIME*. SO WE          
* HAVE CREATED ONE.                                                             
*                                                                               
CURRTIME DC    CL80'@ $G$MODELING_TIME = ''HHMMSS'''                            
         EJECT                                                                  
* THIS MODULE ASSUMES THAT ALL LOAD LIBRARIES FOR THE                           
* SBX ENVIRONMENT ARE PREFIXED BY 'PANAPT.SBX.' !!!                             
SANDBOX  DC    C'PANAPT.SBX.'                                                   
*                                                                               
* THERE NEED TO BE HARD-CODED DD STATEMENTS IN THE INVOKING JCL FOR             
* EACH LOAD LIBRARY USED ACROSS ALL OF OUR DEFINED LIBCODES. WE MIGHT           
* NEED TO EXPAND THIS LIST AT SOME POINT.                                       
*                                                                               
* THE CODE CURRENTLY ASSUMES THAT THERE ARE EXACTLY TWO ENTRIES IN THIS         
* TABLE. IF THERE ARE MORE, IT'S NOT THE END OF THE WORLD. THE TABLE            
* WOULD NEED TO BE SORTED BY VOLSER, TO MAKE IT EASY TO CRUNCH OUT THE          
* DUPLICATES. THAT'S BECAUSE THE DD STATEMENT FOR IEHPROGM WILL NOT             
* PERMIT THE SAME VOLSER TO BE PROVIDED MORE THAN ONCE. SEE THE CODE            
* ABOVE.                                                                        
*                                                                               
LOADLIB_TABLE DS 0D                                                             
LOADLIB  DC    AL1(7),CL8'LOADLIB',CL44'DDS.LOADLIB',C' ',CL6' '                
TESTLIB  DC    AL1(7),CL8'TESTLIB',CL44'DDS.TESTLIB',C' ',CL6' '                
         DC    X'FF'               EOT                                          
         SPACE 3                                                                
MRFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
RVPFILE  DCB   DDNAME=OUTFILE,DSORG=PS,MACRF=PM                                 
*                                                                               
* NOTE THE DDNAME OF "REWORK", WHICH IS VERY CONFUSING. IT REALLY OUGHT         
* TO BE "GLOBALS", BECAUSE THAT'S WHAT IT CONTAINS: GLOBAL PANAPT               
* VARIABLES FOR USE DURING MODELING. (IT'S CALLED "REWORK" FOR A                
* HISTORICAL REASON ONLY: WHEN THIS CONCEPT WAS FIRST IMPLEMENTED,              
* "$G$REWORK" WAS THE ONLY VARIABLE WE NEEDED TO CREATE.) WE DON'T              
* WANT TO CHANGE THE GENERATED JCL AT THIS POINT, SO IT IS STILL CALLED         
* "REWORK".                                                                     
GLOBALS  DCB   DDNAME=REWORK,DSORG=PS,MACRF=PM   ** DDNAME=REWORK !!            
         EJECT                                                                  
         PRINT GEN                                                              
         CNOP  4,8                                                              
APAMMDES_RDW DS F                                                               
         APAMMDES DSECT=NO                                                      
         EJECT                                                                  
         CNOP  4,8                                                              
APAMMVER_RDW DS F                                                               
         APAMMVER DSECT=NO                                                      
         PRINT NOGEN                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSODSPAC                                                         
         DC    C'N'                FORCE DSPACE=N                               
         ORG                                                                    
         EJECT                                                                  
LOADLIB_TABLED DSECT                                                            
LOADLIB_DDNAME_LEN DS AL1          LOAD LIBRARY DDNAME LENGTH                   
LOADLIB_DDNAME     DS CL8          LOAD LIBRARY DDNAME                          
LOADLIB_DSN        DS CL44         LOAD LIBRARY DSN                             
                   DS C            SPACE DELIMITER AFTER DSN                    
LOADLIB_VOLSER     DS CL6          LOAD LIBRARY VOLSER                          
LOADLIB_TABLE_LEN EQU *-LOADLIB_TABLED                                          
         SPACE 3                                                                
         IEFZB4D2                                                               
         IHAPSA                                                                 
         CVT   DSECT=YES                                                        
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
P_LOADLIB_DDNAME DS CL8                                                         
         DS    CL2                                                              
P_LOADLIB_DSN    DS CL44                                                        
         DS    CL2                                                              
P_LOADLIB_VOLSER DS CL6                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015DDPAPTMRR 10/07/19'                                      
         END                                                                    
