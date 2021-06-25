*          DATA SET DDPAPTMRM  AT LEVEL 091 AS OF 07/24/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTMRMA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PANIC                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SMTP                                                                   
*INCLUDE XSORT                                                                  
         TITLE 'ANALYZE PANAPT APTDUMP FILE DATA'                               
PAPTMRM  CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PAPTMRM,=V(REGSAVE),R9                                         
*                                                                               
         LR    R1,RC                                                            
         LARL  RC,COMMWORK         COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         MVC   PARMACT,2(R1)       ACTION (V/M/B)                               
         MVC   PARMLVL,3(R1)       LEVEL (STGE/PROD/ETC)                        
         MVC   PARMENV,7(R1)       ENVIRONMENT (DDS/SBX/ETC)                    
         MVC   PARMUSER,10(R1)     SUBMITTER'S TSO USERID                       
         OC    PARMUSER,SPACES     (SO IDF PARMS WORK ALSO)                     
         MVC   SUBMITTR,PARMUSER                                                
*                                                                               
         MVC   TITLE(30),=C'PANAPT - MEMBERS STATUS UPDATE'                     
         EJECT                                                                  
         OPEN  MRFILE              MOVE REQUEST EXTRACT                         
*                                                                               
         OPEN  (UNLKCRDS,(OUTPUT)) %+UNLOCK STATEMENTS                          
*                                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   MAIN10              NO, IT'S A SUBMIT                            
*&&DO*&& OPEN  NTFYGRPS            EXTRACTED FROM INVENTORY RECORDS             
         OPEN  CURLOUT             STDOUT FROM JIRA REST (CURL) CALL            
         OPEN  (CRDS5955,OUTPUT)   APJP5955 SELECTION CONTROL CARDS...          
*                                  ...FOR REWORK'S "ORIGINAL" MR                
         OPEN  (LOADMODS,OUTPUT)   UPDATED LOAD MODULE NAMES                    
         OPEN  (LOCKCRDS,(OUTPUT)) LOCK CONTROL DATA CARDS                      
*                                                                               
MAIN10   DS    0H                                                               
         SR    R3,R3               # OF MEMBERS                                 
*                                                                               
         MVC   PAPTLEVL,PARMLVL    SAVE THE LEVEL                               
         MVC   PENVIRON,PARMENV    SAVE THE ENVIRONMENT (!)                     
*                                                                               
* CLEAR THE START OF SOME SELECTED FIELDS SO THAT WE DON'T SEND THEM            
* DURING A 'SUB' ACTION.                                                        
*                                                                               
         MVI   PAPPRVR,0           APPROVER, DATE & TIME                        
         MVI   PPDATE,0                                                         
         MVI   PPTIME,0                                                         
         MVI   PNOTIFYR,0                                                       
*                                                                               
         MVIY  PMEMBER,0           MEMBER TABLE FIELDS                          
         MVIY  PLIBCODE,0                                                       
         MVIY  PTOUSRDT,0                                                       
         MVIY  POBJECT,0                                                        
         MVIY  PTOBJECT,0                                                       
         MVIY  PPLVL,0                                                          
         MVIY  PPUSR,0                                                          
         MVIY  PPDTTMU,0                                                        
         MVIY  PREVMR#,0                                                        
         EJECT                                                                  
NEXT     LARL  R2,REC                                                           
         GET   MRFILE,(R2)                                                      
*                                                                               
         LARL  R6,REC+4                                                         
         USING APAMMDES,R6                                                      
         CLC   DESTYPE,=C'01'      MR DESCRIPTION RECORD?                       
         BNE   TYPE02              NO                                           
*                                                                               
         MVC   PMR#,DESNUMB        MR NUMBER                                    
         MVC   LEVELPOS,DESCLPOS   CURRENT LEVEL RELATIVE POSITION              
*                                                                               
         MVC   PACTION,=CL12'PROMOTION'   ASSUME PROMOTION                      
         CLI   PARMACT,BACKOUT                                                  
         BNE   *+10                                                             
         MVC   PACTION,=CL12'BACKOUT'     IT'S A BACKOUT                        
*                                                                               
         MVC   PADATMON,DESUPDMM   LAST UPDATE DATE/TIME                        
         MVC   PADATDAY,DESUPDDD                                                
         MVC   PADATYR,DESUPDCC                                                 
         MVC   PATIMHR,DESUPDHH                                                 
         MVC   PATIMMIN,DESUPDMI                                                
         MVC   PATIMSEC,DESUPDSS                                                
*                                                                               
         LA    R4,PADATE                                                        
         LA    R5,PATIME                                                        
         BRAS  RE,REALTIME         CONVERT SYS DATE&TIME TO REAL TIME           
*                                                                               
         MVC   PCDATMON,DESADDMM   MR CREATION DATE/TIME                        
         MVC   PCDATDAY,DESADDDD                                                
         MVC   PCDATYR,DESADDCC                                                 
         MVC   PCTIMHR,DESADDHH                                                 
         MVC   PCTIMMIN,DESADDMN                                                
         MVC   PCTIMSEC,DESADDSS                                                
*                                                                               
         LA    R4,PCDATE                                                        
         LA    R5,PCTIME                                                        
         BRAS  RE,REALTIME         CONVERT SYS DATE&TIME TO REAL TIME           
*                                                                               
         MVC   POWNER,DESADDID     MR OWNER                                     
         MVC   PSTA,DESSTAT        MR STATUS                                    
         MVC   PTICKET,DESSRREQ    MR TICKET # ("SERVICE REQUEST")              
         MVC   PDES,DESDESCR       MR DESCRIPTION                               
         MVC   PMOVETYP,DESMTYPE   MR MOVE TYPE                                 
         MVC   PEARSTOP,DESESTOP   MR EARLY STOP LEVEL                          
*                                                                               
         CLC   =C'RW ',DESNAME     IS THIS A COPY FOR REWORK MR?                
         BNE   TYPE01G                                                          
         CLI   DESNAME_DELIMITER,C':'                                           
         BNE   TYPE01G             NO                                           
         MVC   PORIGMR#,DESNAME_ORIG_MR#  YES: SAVE ORIGINAL MR NUMBER          
*                                                                               
TYPE01G  DS    0H                                                               
*                                                                               
* NOTE: WE READ FROM A DFSORT OUTPUT FILE, WHICH WAS GENERATED VIA THE          
*       PANAPT APCS5103 CONTROL FILE REPORT. THE FILE CONTAINS THE              
*       DEFINED PANAPT LEVELS IN THE SYSTEM (THEIR "SHORT NAMES" AND            
*       ABBREVATIONS). THE APCS5103 SYSPRINT REPORT IS NOT A DOCUMENTED         
*       API, BUT THE ONLY ALTERNATIVE TO USING IT WOULD BE TO HARD-CODE         
*       A TABLE OF LEVELS IN THIS PROGRAM.                                      
*                                                                               
         OPEN  LEVELS              DEFINED LEVELS (FROM APCS5103)               
*                                                                               
         LA    R4,P                                                             
         USING APCS5103_VIA_DFSORT_DSECT,R4                                     
*                                                                               
         GET   LEVELS,P            1ST 2 LEVELS ARE FOR RESERVED FOR...         
         GET   LEVELS,P            ...THE DEVELOPMENT FACILITY. IGNORE.         
*                                                                               
         GET   LEVELS,P            THIS IS THE THIRD DEFINED LEVEL              
         CLC   =C'TEST',APCS5103_LVLSHORT   IT HAD BETTER BE "TEST"!            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,LEVELTAB         INTERNAL TABLE OF PANAPT LEVELS              
         USING LEVELTBD,R5                                                      
         LHI   R7,1                                                             
TYPE01J  DS    0H                                                               
         MVC   LVLSHORT,APCS5103_LVLSHORT  SHORT LEVEL NAME                     
         MVC   LVLABBRV,APCS5103_LVLABBRV  LEVEL ABBREVIATION                   
         STCM  R7,3,LVLSPS         LEVEL RELATIVE SYSTEM POSITION               
         MVI   LVLREQD,C'N'        ASSUME LEVEL IS NOT REQUIRED                 
         MVI   LVLAVREQ,C'N'       ASSUME NO VERIFIC./APPROVALS REQ'D           
         MVC   LVLDATEM,SPACES     ASSUME NO PROMOTION HISTORY                  
         CLC   =C'TEST',APCS5103_LVLSHORT  IS THIS THE "TEST" LEVEL?            
         BNE   *+14                NO                                           
         MVI   LVLREQD,C'Y'        TEST LEVEL IS REQUIRED                       
         XC    LVLDATEM,LVLDATEM   TEST LEVEL PROMOTION IS ASSUMED              
*                                                                               
         CLC   =C'PROD',APCS5103_LVLSHORT "PROD" IS ALWAYS LAST LEVEL           
         BE    TYPE01K                                                          
         LA    R5,LEVELTBQ(R5)     BUMP TO NEXT PROMOTION HISTORY LEVEL         
         AHI   R7,1                BUMP RELATIVE LEVEL COUNT                    
         GET   LEVELS,P                                                         
         B     TYPE01J                                                          
*                                                                               
         DROP  R4                                                               
         DROP  R5                                                               
TYPE01K  DS    0H                                                               
         CLOSE LEVELS                                                           
         MVC   P,SPACES            CLEAR FIELD "P" FOR NEXT PRINT CALL          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DESCMOVC       COMPLETED MOVE LEVEL COUNT                   
         BZ    TYPE01N             NO MOVES YET                                 
*                                                                               
* REMEMBER ALL PANAPT LEVELS TO WHICH THIS MR WAS PROMOTED. THIS INFO           
* WILL BE USED LATER TO CONFIRM THAT THE CURRENT MR STATUS IS CORRECT.          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DESEXCNT       # EXPANDED DESCRIPTION RECORDS               
         MHI   RF,L'DESXPDES       R0 = # BYTES USED FOR EXP. DESC.             
         LA    RF,DESXPDES(RF)     RF = A(DESMRMVC AREA)                        
M        USING DESMRCID,RF                                                      
         LA    RE,LEVELTAB         TABLE OF PANAPT LEVELS                       
         USING LEVELTBD,RE                                                      
TYPE01L  DS    0H                                                               
         CLC   M.DESMRCSN,LVLSHORT MATCH ON SHORT LEVEL NAME?                   
         BE    *+18                                                             
         LA    RE,LEVELTBQ(RE)     NO: BUMP TO NEXT LEVEL                       
         CLI   0(RE),0                                                          
         BNE   TYPE01L                                                          
         DC    H'0'                INTERNAL LEVELTAB IS HORRIBLY WRONG          
         MVC   LVLDATEM,M.DESMRCDM MOVE DATE TO THIS LEVEL                      
*                                                                               
         LA    RF,L'DESMRMVC(RF)   BUMP TO NEXT PROMOTION HISTORY ENTRY         
         BCT   R0,TYPE01L                                                       
         DROP  M                                                                
         DROP  RE                                                               
*                                                                               
TYPE01N  DS    0H                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   TYPE01X             NO, IT'S A SUBMIT                            
*                                                                               
* BUILD APJP5955 CONTROL CARDS FOR A SUBSEQUENT MOVE REQUEST EXTRACT.           
* THIS IS DONE FOR ONLY ONE REASON: TO FAIL THE VERIFICATION OF THIS            
* MOVE REQUEST IF IT IS A REWORK, AND IF IT'S ORIGINAL WAS BACKED OUT.          
* THE CONTROL CARDS ARE BUILT IN SUCH A WAY THAT NO MATTER WHAT, WE             
* KNOW THAT IF THE APJP5955 OUTPUT FILE ENDS UP EMPTY, WE'RE GOOD.              
* OTHERWISE, IT'S BAD!                                                          
*                                                                               
         MVC   RECORD,=CL80'ACTION="DUMPPEND"'                                  
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="MVP-B,MVS-B,MVPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="AEP-B,AES-B,AEPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="AMP-B,AMS-B,AMPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="APP-B,APS-B,APPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="AWP-B,AWS-B,AWPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'STATUS="SLP-B,SLS-B,SLPS-B"'                        
         PUT   CRDS5955,RECORD                                                  
         MVC   RECORD,=CL80'MR="######"'                                        
         MVC   RECORD+4(6),DESNUMB DEFAULT TO THIS MR#                          
         CLC   PORIGMR#,SPACES     IS THIS A COPY FOR REWORK?                   
         BE    *+10                                                             
         MVC   RECORD+4(6),PORIGMR#  NO: EXTRACT THE ORIGINAL LATER ON          
         PUT   CRDS5955,RECORD                                                  
*                                                                               
* DEIS AUG/2019:                                                                
* "ADDITIONAL NOTIFICATIONS" WERE SUPPORTED IN LOTUS NOTES, BUT THEY            
* ARE NOT SUPPORTED IN JIRA (BECAUSE JIRA ITSELF USES "WATCHERS" FOR            
* THIS PURPOSE).                                                                
*                                                                               
*&&DO*&& BRAS  RE,NOTIFIES         PROCESS ADDITIONAL NOTIFICATIONS             
*                                                                               
         BRAS  RE,VALTKT#          VALIDATE JIRA TICKET NUMBER                  
         LTR   RF,RF               VALID TICKET NUMBER PRESENT?                 
         BZ    TYPE01S             YES                                          
         CLC   PARMENV,=C'SBX'     ARE WE IN THE SANDBOX?                       
         BE    TYPE01S             YES: TICKET NUMBER NOT REQUIRED              
*                                                                               
         ST    RF,RETCODE          NO: SET RETURN CODE                          
         BRAS  RE,SNDMAIL                                                       
         CHI   RF,8                ERROR?                                       
         BE    CLOSE               YES: NO NEED TO DO ANYTHING ELSE             
*                                                                               
TYPE01S  DS    0H                                                               
*                                                                               
* THE "GENERAL VERIFICATION" MUST OCCUR AFTER ALL OTHER VERIFICATIONS           
* HAVE OCCURRED. SO WE MUST MAKE SURE THAT ALL OTHER REQUIRED                   
* VERIFICATIONS FOR THE CURRENT LEVEL HAVE BEEN OBTAINED BEFORE                 
* VERIFYING THIS CATEGORY.                                                      
*                                                                               
         CLC   DESCLVNF(6),DESCLVSF      CHECK ALL VERIF. EXCEPT '07'           
         BNE   *+14                                                             
         CLC   DESCLVNF+7(13),DESCLVSF+7                                        
         BE    TYPE01T             ALL HAVE COMPLETED SUCCESSFULLY              
         MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIFICATION FAILURE)         
         MVC   MESSAGE1,MSG02_1    REQUIRED VERIFICATION(S) MISSING             
         MVC   MESSAGE2,MSG02_2                                                 
         BRAS  RE,SNDMAIL                                                       
         B     CLOSE               NO NEED TO DO ANYTHING ELSE                  
*                                                                               
TYPE01T  DS    0H                                                               
*                                                                               
* THERE IS A LIMIT TO THE NUMBER OF MEMBERS WHICH THIS PROGRAM CAN              
* HANDLE WITHIN A GIVEN MOVE REQUEST (BECAUSE A TABLE OF FIXED SIZE IS          
* BUILT BY THIS PROGRAM TO HOLD OF ALL THE MEMBER INFORMATION).                 
* WE USED TO PERMIT AN UNLIMITED NUMBER OF MEMBERS, AND SIMPLY GENERATE         
* A WARNING IF THE MAXIMUM WAS EXCEEDED. BUT NOW, WE *FAIL* IF THE              
* MAXIMUM IS EXCEEDED (IT'S A VERY GENEROUS NUMBER). THIS MAKES THINGS          
* SIMPLER, BECAUSE WE DON'T HAVE TO WORRY ABOUT THIS EXCEPTIONAL CASE.          
* IF WE EVER NEED TO SUPPORT MORE MEMBERS, WE CAN INCREASE MAXMEMQ.             
*                                                                               
         IF (CLC,DES#MEM,H,=A(MAXMEMQ)) MAX NUMBER OF MEMBERS EXCEEDED?         
           MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIF. FAILURE)             
           MVC   MESSAGE1,MSG09_1    TOO MANY MEMBERS FOR THIS PROGRAM          
           LHI   RE,MAXMEMQ                                                     
           EDIT  (RE),(4,MESSAGE1+59)                                           
           MVC   MESSAGE2,MSG09_2                                               
           MVI   CC_ADMIN,C'Y'       SEND E-MAIL COPY TO ADMINISTRATOR          
           BRAS  RE,SNDMAIL                                                     
           B     CLOSE               NO NEED TO DO ANYTHING ELSE                
         ENDIF ,                                                                
*                                                                               
         CLI   PMOVETYP,C'T'       MOVE TYPE "T" (TEMPORARY)?                   
         BNE   TYPE01X                                                          
         CLC   PARMLVL,=C'STGE'    YES: RVP FOR STGE LEVEL?                     
         BE    TYPE01X                                                          
*                                                                               
         MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIFICATION FAILURE)         
         MVC   MESSAGE1,MSG03_1    TEMPORARY MOVE REQUEST                       
         MVC   MESSAGE2,MSG03_2                                                 
         BRAS  RE,SNDMAIL                                                       
         B     CLOSE               NO NEED TO DO ANYTHING ELSE                  
*                                                                               
TYPE01X  DS    0H                                                               
         B     NEXT                                                             
         DROP  R6                                                               
         EJECT                                                                  
TYPE02   DS    0H                                                               
         LARL  R6,REC+4                                                         
         USING MMMBRD,R6                                                        
         CLC   MRTYPE,=C'02'       MEMBER RECORD?                               
         BNE   TYPE03                                                           
*                                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BE    TYPE02A6            YES: PROCESS THE MEMBERS                     
         CLI   PARMACT,PROMOTE     IS THIS A MR PROMOTION?                      
         BE    TYPE02A6            YES: PROCESS THE MEMBERS                     
*                                                                               
         CLI   PARMACT,BACKOUT     BACKOUT?                                     
         JNE   *+2                 INVALID ACTION ?!?                           
         CLC   PARMLVL,=C'STGE'    BACKOUT OF STGE?                             
         BNE   TYPE02X             NO: IGNORE THE '02' RECORD                   
*                                                                               
* IF THIS IS A BACKOUT FROM STGE, AND IT'S AN ACAT MEMBER, WE NEED TO           
* GET THE CATALP NAME SO WE CAN GENERATE A ++UNLOCK STATEMENT FOR IT.           
* (GEN MEMBERS ARE SIMPLER, BECAUSE WE CAN DERIVE THE SCREEN DSECT              
* NAME FROM THE SOURCE MEMBER NAME ITSELF: JUST APPEND A "D".)                  
* NOTE: A BACKOUT FROM STGE IS CURRENTLY THE ONLY SCENARIO IN WHICH             
* PANDD1 IS *NOT* DYNAMICALLY ALLOCATED BY THIS PROGRAM. (THE                   
* ALLOCATION HAPPENS VIA A DD STATEMENT IN PANAPT MODEL DDJMCRPT.)              
*                                                                               
         CLC   =C'ACAT',MRLIB      ACAT MEMBER?                                 
         BNE   TYPE02A4                                                         
*                                  YES: DIG OUT THE CATALP NAME                 
         GOTO1 =V(PANIC),DMCB,(X'10',=C'READ'),=C'DIR',MRFRMEM,RECORD, +        
               A(AUDITINF)                                                      
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BNE   TYPE02X             NO: SKIP THIS MEMBER                         
         BRAS  RE,GETPHASE         DERIVE CATALP NAME                           
*                                                                               
* CLOSE AND DYNAMICALLY UNALLOCATE PANDD1                                       
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
*                                                                               
TYPE02A4 DS    0H                                                               
         BRAS  RE,GENUNLK          GENERATE UNLOCK CONTROL CARD(S)              
         B     TYPE02X                                                          
*                                                                               
TYPE02A6 DS    0H                                                               
         MVC   MEMRECMR,SPACES     MR THAT DID THE PRIOR PROMOTION              
         MVC   MEMITMF#,SPACES     ITMF ISSUE # FOR PRIOR PROMOTION             
*                                                                               
* THE LIBCODES DATASET IS PRODUCED VIA A PRECEDING JOB STEP, WHICH              
* INVOKES THE PANAPT APJP5970 PROC TO PERFORM A LIBCODE EXTRACT.                
*                                                                               
         OPEN  LIBCODES                                                         
TYPE02B  DS    0H                                                               
         LARL  R4,LIBCRECR                                                      
         GET   LIBCODES,(R4)                                                    
         AHI   R4,4                BUMP PAST RDW                                
         USING APAMLIB2,R4                                                      
         CLC   L2LIBS,MRLIBC       FIND MATCHING LIBCODE RECORD                 
         BNE   TYPE02B                                                          
*                                                                               
* THE LIBCODE GROUP CONSISTENCY CHECK ALSO OCCURS IN DDPAPTMEX (THE             
* PANVALET MEMBER EXISTENCE EXIT). WE DO IT THERE BECAUSE IT'S NICE TO          
* NOTIFY THE PROGRAMMER AT MR CLOSE TIME IF THERE'S A PROBLEM. HOWEVER,         
* SINCE IT'S POSSIBLE THAT A GIVEN LIBCODE MAY NOT USE THAT PARTICULAR          
* EXIT (E.G., WARN/BAK), WE NEED TO REDO THE CHECK HERE, AND VALIDATE           
* THE LIBCODE CONSISTENCY ACROSS EVERY MEMBER IN THE MOVE REQUEST.              
*                                                                               
         CLI   PRKSFLAG,C' '       /K CONSISTENCY FLAG IS ALREADY SET?          
         BNE   TYPE02B2            YES                                          
         MVI   PRKSFLAG,C'N'       ASSUME THIS LIBCODE ISN'T /K                 
         CLI   MRSUB,C'K'          IS THIS A /K LIBCODE?                        
         BNE   *+8                                                              
         MVI   PRKSFLAG,C'K'       YES                                          
         B     TYPE02B4                                                         
*                                                                               
TYPE02B2 DS    0H                                                               
         MVI   BYTE,C'N'           ASSUME THIS ISN'T A /K LIBCODE               
         CLI   MRSUB,C'K'          IS IT?                                       
         BNE   *+8                                                              
         MVI   BYTE,C'K'           YES                                          
         CLC   =C'WARNBAK',MRLIBC  LIBCODE WARN/BAK?                            
         BNE   *+8                                                              
         MVI   BYTE,C'K'           YES: THIS IS OKAY WITH /K LIBCODES           
         CLC   PRKSFLAG,BYTE       IS THIS CONSISTENT WITH PREVIOUS?            
         BNE   TYPE02B8            NO: LIBCODES ARE INCONSISTENT                
*                                                                               
TYPE02B4 DS    0H                                                               
         CLC   =C'@* LIBCODE GROUP = "',L2MOD1  "LIBCODE GROUP" RECORD?         
         BNE   TYPE02C             NO: ASSUME NO CONSISTENCY ISSUES             
         CLC   LIBCDGRP,SPACES     FIRST LIBCODE SEEN YET?                      
         BNE   *+14                YES: CHECK CONSISTENCY                       
         MVC   LIBCDGRP,L2MOD1+20  NO: SAVE THIS GROUP VALUE                    
         B     TYPE02C                                                          
         CLC   LIBCDGRP,L2MOD1+20  IS THIS IN THE SAME LIBCODE GROUP?           
         BE    TYPE02C             YES: LIBCODES ARE CONSISTENT                 
*                                                                               
TYPE02B8 DS    0H                                                               
         MVC   RETCODE,=F'8'       SET RC=8: VERIFICATION FAILURE               
         MVC   MESSAGE1,MSG04_1    INCONSISTENT LIBCODES                        
         MVC   MESSAGE2,MSG04_2                                                 
         BRAS  RE,SNDMAIL                                                       
         B     TYPE02W             NO NEED TO DO ANYTHING ELSE                  
*                                                                               
TYPE02C  DS    0H                                                               
         LR    RF,R4                                                            
         AHI   RF,4096                                                          
         USING APAMLIB2+4096,RF                                                 
*                                                                               
         XC    RELATLBC,RELATLBC   ASSUME NO RELATED LIBCODE                    
         CLC   =C'ACAT',L2LIBS     ACAT MEMBERS GENERATE OBJECT MODULES         
         BNE   *+10                                                             
         MVC   RELATLBC,L2RELOBJ   EXTRACT THE RELATED OBJECT LIBCODE           
         CLC   =C'GEN',L2LIBS      GEN MEMBERS GENERATE SCREEN DSECTS           
         BNE   *+10                                                             
         MVC   RELATLBC,L2RELSRC   EXTRACT THE RELATED SOURCE LIBCODE           
*                                                                               
         AHI   RF,4096                                                          
         USING APAMLIB2+8192,RF                                                 
         SR    R0,R0                                                            
         ICM   R0,3,L2LVLCT        NUMBER OF LEVELS IN THIS LIBCODE             
         DROP  RF                                                               
*                                                                               
         AHI   R4,L2LVLDTA-APAMLIB2                                             
         USING L2LVLDAT,R4                                                      
         CLC   L2LVLSNM,=C'TEST'   CHECK SHORT LEVEL NAME                       
         BE    *+6                                                              
         DC    H'0'                'TEST' IS ALWAYS THE FIRST LEVEL!            
         ST    R4,AFRSTLVL         SAVE A(FIRST LEVEL DATA)                     
*                                                                               
* THIS IS A PANAPT LIMITATION WORKAROUND. IF A MR IS CHANGED AFTER IT'S         
* ALREADY IN-FLIGHT, AND THE LIBCODE ON A MEMBER IS CHANGED IN SUCH A           
* WAY THAT IT CAUSES A NEW LEVEL TO BE INTRODUCED OR REMOVED FROM THE           
* MIGRATION PATHWAY, THEN PANAPT WILL PERMIT THE CHANGE, BUT IT WILL            
* NOT UPDATE THE STATUS OF THE MR TO REFLECT THE CORRECT LEVEL. FOR             
* EXAMPLE, IF THE MR HAS AN ALNK MEMBER, AND IS PROMOTED TO STGE, AND           
* THE MEMBER'S LIBCODE IS THEN CHANGED TO ALNK/K, PANAPT WON'T CHANGE           
* THE STATUS OF THE MR FROM AWP TO AWPS. THEREFORE, WE BUILD A TABLE OF         
* LEVELS (BASED ON THE LIBCODE EXTRACT AND MEMBER LIBCODES IN THE MR)           
* WHICH WILL BE USED AT THE END OF THIS PROGRAM TO FIGURE OUT WHAT THE          
* CURRENT LEVEL REALLY SHOULD BE.                                               
*                                                                               
TYPE02D  DS    0H                                                               
         CLI   L2LVLSTA,L2LVLACT   LEVEL IS ACTIVE?                             
         BNE   TYPE02F             NO: SKIP TO NEXT DEFINED LEVEL               
*                                                                               
         LA    R5,LEVELTAB         TABLE OF PANAPT LEVELS                       
         USING LEVELTBD,R5                                                      
*                                                                               
         CLC   L2LVLSNM,LVLSHORT   MATCH ON SHORT LEVEL NAME?                   
         BE    *+18                                                             
         LA    R5,LEVELTBQ(R5)     NO: BUMP TO NEXT LEVEL                       
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
         DC    H'0'                INTERNAL LEVELTAB IS HORRIBLY WRONG          
*                                                                               
         MVI   LVLREQD,C'Y'        THIS LEVEL IS REQUIRED FOR THIS MR           
*                                                                               
         LA    RE,L2PAPP           APPROVALS REQUIRED ARRAY                     
         LHI   R1,L'L2PAPP                                                      
         CLI   0(RE),C'Y'          APPROVAL REQUIRED?                           
         BE    TYPE02E             YES                                          
         BCT   R1,*-8                                                           
         LA    RE,L2VPAPP          VERIFICATIONS REQUIRED ARRAY                 
         LHI   R1,L'L2VPAPP                                                     
         CLI   0(RE),C'Y'          VERIFICATION REQUIRED?                       
         BE    TYPE02E             YES                                          
         BCT   R1,*-8                                                           
         B     TYPE02F             NEITHER RVP NOR APPROVAL REQUIRED            
*                                                                               
TYPE02E  DS    0H                                                               
         MVI   LVLAVREQ,C'Y'       RVP(S) AND/OR APPROVAL(S) REQUIRED           
         DROP  R5                                                               
*                                                                               
TYPE02F  DS    0H                                                               
         ST    R4,ALASTLVL         SAVE THE CURRENT LEVEL POINTER               
         LA    R4,L2NXTLVL         BUMP TO NEXT LEVEL                           
         BCT   R0,TYPE02D          PROCESS ALL LEVELS                           
*                                                                               
         IF (CLI,PARMACT,EQ,VERIFY),ORIF,   IS THIS AN RVP...                   
            (CLI,PARMACT,EQ,PROMOTE),AND,   ...OR A MOVE TO STGE?               
            (CLC,PARMLVL,EQ,=C'STGE')                                           
           L     R4,AFRSTLVL         YES: RESTORE A(FIRST LEVEL DATA)           
         ELSE  ,                                                                
           L     R4,ALASTLVL         NO: USE THE LAST (PROD) LEVEL              
         ENDIF ,                                                                
*                                                                               
         MVC   ORIGDSN,L2PRDSN     ORIGIN DSN FROM LIBCODE DEFINITION           
         CLC   L2PRDDN,=C'$USERLIB' USE PERSONAL PAN LIBRARY?                   
         BNE   TYPE02H              NO                                          
*                                                                               
         MVC   ORIGDSN,SPACES                                                   
         MVC   ORIGDSN(4),=C'PAN.' STANDARD PERSONAL PANLIB HLQ                 
         LA    RF,ORIGDSN+4        POINT RF TO NEXT CHARACTER                   
         CLC   PARMENV,=C'SBX'                                                  
         BNE   *+14                                                             
         MVC   ORIGDSN(15),=C'PANAPT.SBX.PAN.' SBX PERSONAL PANLIB HLQS         
         LA    RF,ORIGDSN+15       POINT RF TO NEXT CHARACTER                   
*                                                                               
         LA    RE,POWNER                                                        
         MVC   0(1,RF),0(RE)       NEXT QUALIFIER IS MR OWNER'S USERID          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVC   0(8,RF),=C'.LIBRARY' LAST QUALIFIER IS '.LIBRARY'                
*                                                                               
TYPE02H  DS    0H                                                               
         DO WHILE=(CLC,L2LVLSNM,NE,=C'PROD') FIND PROD SHORT LEVEL NAME         
           LA    R4,L2NXTLVL         BUMP TO NEXT LEVEL'S DATA                  
         ENDDO ,                                                                
*                                                                               
         IF (CLI,PARMACT,EQ,VERIFY),ORIF,   IS THIS AN RVP...                   
            (CLI,PARMACT,EQ,PROMOTE),AND,   ...OR A MOVE TO STGE?               
            (CLC,PARMLVL,EQ,=C'STGE')                                           
           MVC   PRODDSN,L2PRDSN     YES: USE PROD PANLIB DSN                   
           MVC   ACCSMETH,L2PRACC         AND PROD ACCESS METHOD                
         ELSE  ,                                                                
           MVC   PRODDSN,L2BKDSN     NO:  USE BKUP PANLIB DSN                   
           MVC   ACCSMETH,L2BKACC         USE BKUP ACCESS METHOD                
         ENDIF ,                                                                
*                                                                               
         OC    RELATLBC,RELATLBC   ANY RELATED LIBCODE TO EXTRACT?              
         BZ    TYPE02J             NO                                           
*                                                                               
TYPE02I  DS    0H                                                               
         LARL  R4,LIBCRECR                                                      
         GET   LIBCODES,(R4)                                                    
         AHI   R4,4                BUMP PAST RDW                                
         USING APAMLIB2,R4                                                      
         CLC   L2LIBS,RELATLBC     FIND MATCHING RELATED LIBCODE RECORD         
         BNE   TYPE02I                                                          
*                                                                               
         AHI   R4,L2LVLDTA-APAMLIB2                                             
         USING L2LVLDAT,R4                                                      
         CLC   L2LVLSNM,=C'TEST'   CHECK SHORT LEVEL NAME                       
         BE    *+6                                                              
         DC    H'0'                'TEST' IS ALWAYS THE FIRST LEVEL!            
*                                                                               
         MVC   RELATDSN,L2PRDSN    RELATED LIBCODE DSN FROM DEFINITION          
         CLC   L2PRDDN,=C'$USERLIB' USE PERSONAL PAN LIBRARY?                   
         BNE   TYPE02J              NO                                          
*                                                                               
         MVC   RELATDSN,SPACES                                                  
         MVC   RELATDSN(4),=C'PAN.' STANDARD PERSONAL PANLIB HLQ                
         LA    RF,RELATDSN+4       POINT RF TO NEXT CHARACTER                   
         CLC   PARMENV,=C'SBX'                                                  
         BNE   *+14                                                             
         MVC   RELATDSN(15),=C'PANAPT.SBX.PAN.' SBX PERS PANLIB HLQS            
         LA    RF,RELATDSN+15      POINT RF TO NEXT CHARACTER                   
*                                                                               
         LA    RE,POWNER                                                        
         MVC   0(1,RF),0(RE)       NEXT QUALIFIER IS MR OWNER'S USERID          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
         MVC   0(8,RF),=C'.LIBRARY' LAST QUALIFIER IS '.LIBRARY'                
*                                                                               
TYPE02J  DS    0H                                                               
         DROP  R4                                                               
*                                                                               
         CLC   ACCSMETH,=C'ID'     DUMMY MEMBER WITHOUT INVENTORY?              
         BE    TYPE02K             CORRECT: PROCESS THE "MEMBER"                
*                                                                               
* NOTE: THE MRCDFLG FIELD IN THE MEMBER RECORD IS DOCUMENTED AS BEING           
* THE CONCURRENT DEVELOPMENT FLAG. UNFORTUNATELY, THERE IS NO                   
* DOCUMENTATION IN THE APAMMMBR DSECT (OR ELSEWHERE) OF THE *VALUE* OF          
* THAT FIELD WHEN THE FLAG IS ON. EMPIRICAL OBSERVATION HAS SHOWN THAT          
* A "Y" IN THAT FIELD MEANS THAT THE "C" FLAG IS ON, SO THAT'S WHAT'S           
* BEING TESTED HERE.                                                            
*                                                                               
         CLI   MRASSNFL,MRASSNAW   IS "A" FLAG POSTED ON THE MEMBER?            
         BE    *+12                YES: IT'S ASSIGNED TO ANOTHER MR             
         CLI   MRCDFLG,C'Y'        IS "C" FLAG POSTED ON THE MEMBER?            
         BNE   TYPE02K             NO: SO FAR, SO GOOD                          
*                                                                               
         MVC   RETCODE,=F'8'       SET RC=8: VERIFICATION FAILURE               
         MVC   MESSAGE1,MSG05_1    "A" OR "C" FLAG(S) FOUND                     
         MVC   MESSAGE2,MSG05_2                                                 
         BRAS  RE,SNDMAIL                                                       
         B     TYPE02W             NO NEED TO DO ANYTHING ELSE                  
*                                                                               
TYPE02K  DS    0H                                                               
*                                                                               
* IF THIS IS AN SUB FOR PROD, AND IF THE MR WAS PROMOTED VIA THE PRKS           
* LEVEL, THEN THE PAN MEMBERS HAVE ALREADY BEEN MOVED TO THE PRODUCTION         
* PAN LIBRARY. THAT MEANS THAT THERE IS NO NEED TO UPDATE THE MEMBER            
* TABLE, BECAUSE WE DID SO DURING THE SUB FOR PRKS.                             
*                                                                               
         CLC   PARMLVL,=C'STGE'    RVP OR PROMOTION TO STGE?                    
         BNE   *+12                                                             
         MVI   PUPDMTAB,C'Y'       YES: WE MIGHT UPDATE MEMBER TABLE            
         B     TYPE02M                                                          
*                                                                               
         CLC   PARMLVL,=C'PRKS'    RVP OR PROMOTION TO PRKS?                    
         BNE   *+12                                                             
         MVI   PUPDMTAB,C'Y'       YES: WE MIGHT UPDATE MEMBER TABLE            
         B     TYPE02M                                                          
*                                                                               
         CLC   PARMLVL,=C'PROD'    RVP OR PROMOTION TO PROD?                    
         JNE   *+2                 INVALID LEVEL ?!?                            
         CLC   MRSUB,=C'K  '       YES: IS THIS A /K LIBCODE?                   
         BE    TYPE02W                                                          
         MVI   PUPDMTAB,C'Y'       NO: WE MIGHT UPDATE MEMBER TABLE             
*                                                                               
TYPE02M  DS    0H                                                               
         AHI   R3,1                INCREMENT NUMBER OF MEMBERS IN MR            
*                                                                               
         CLC   ACCSMETH,=C'PV'     LIBCODE USES PANVALET ACCESS METHOD?         
         BNE   TYPE02S             NO: NO PAN MEMBER FOR THIS LIBCODE           
*                                                                               
* DYNAMICALLY ALLOCATE PANDD1 TO THE ORIGIN PAN LIBRARY                         
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'FF',=C'PANDD1  '),ORIGDSN                   
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'10',=C'READ'),=C'DIR',MRFRMEM,RECORD, +        
               A(AUDITINF)                                                      
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BE    TYPE02O             YES                                          
*                                                                               
         MVC   RETCODE,=F'8'       SET RC=8: VERIFICATION FAILURE               
         MVC   MESSAGE1,MSG06_1    MEMBER NOT FOUND                             
         MVC   MESSAGE1+29(L'MRFRMEM),MRFRMEM  MEMBER NAME                      
         MVC   MESSAGE2,SPACES                                                  
         MVC   MESSAGE2(L'ORIGDSN),ORIGDSN  ORIGIN PAN LIBRARY DSN              
         BRAS  RE,SNDMAIL                                                       
         B     TYPE02W             NO NEED TO DO ANYTHING ELSE                  
*                                                                               
TYPE02O  DS    0H                                                               
         BRAS  RE,GETPHASE         DERIVE MEMBER'S OUTPUT OBJECT NAME           
*                                                                               
* CLOSE AND DYNAMICALLY UNALLOCATE PANDD1                                       
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
*                                                                               
         CLI   ERRFLAG,C'Y'        WAS A VERIFICATION ERROR FOUND?              
         BNE   *+12                NO                                           
         BRAS  RE,SNDMAIL                                                       
         B     TYPE02W             NO NEED TO DO ANYTHING ELSE                  
*                                                                               
* DYNAMICALLY ALLOCATE PANDD1 TO THE APPROPRIATE PAN LIBRARY. NORMALLY          
* THIS IS THE PRODUCTION LIBRARY, BUT IN THE CASE OF APPL/DEL, THE              
* TARGET LIBRARY IS ACTUALLY THE BKUP LIBRARY (SEE THE APPL/DEL LIBCODE         
* DEFINITION IN THE PANAPT CONTROL FILE).                                       
*                                                                               
         IF (CLC,MRLIBC,EQ,=C'APPLDEL')                                         
           GOTO1 =V(DYNALLOC),DMCB,(X'FF',=C'PANDD1  '),ORIGDSN                 
         ELSE ,                                                                 
           GOTO1 =V(DYNALLOC),DMCB,(X'FF',=C'PANDD1  '),PRODDSN                 
         ENDIF ,                                                                
*                                                                               
         GOTO1 =V(PANIC),DMCB,(X'30',=C'READ'),=C'DIR',MRFRMEM,RECORD, +        
               A(AUDITINF)                                                      
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BNE   TYPE02Q             NO: IT'S A NEW PAN MEMBER                    
*                                                                               
         LARL  R4,AUDITINF         PICK UP AUDIT DATA                           
         USING AUDITD,R4                                                        
         CLC   =C'MR#',MR#TAG      MAKE SURE THIS IS THE MR NUMBER              
         BNE   *+10                                                             
         MVC   MEMRECMR,MR#        MR# THAT LAST PROMOTED THE MEMBER            
         CLC   =C'ITMF-',MR#ITMF#  MAKE SURE THIS IS A JIRA ITMF ISSUE          
         BNE   *+10                                                             
         MVC   MEMITMF#,MR#ITMF#   JIRA ISSUE THAT LAST PROMOTED MEMBER         
         DROP  R4                                                               
*                                                                               
TYPE02Q  DS    0H                                                               
* CLOSE AND DYNAMICALLY UNALLOCATE PANDD1                                       
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
*                                                                               
         IF (CLI,PARMACT,EQ,VERIFY),AND, IS THIS AN RVP...                      
            (CLC,MRLIBC,NE,=C'APPLDEL')  ...BUT NOT FOR APPL/DEL?               
           BRAS  RE,GENLOCK          YES: GENERATE LOCK CONTROL CARD(S)         
         ENDIF ,                                                                
*                                                                               
TYPE02S  DS    0H                                                               
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,L'PMEMBER                                                     
         A     RE,=A(PMEMBER)                                                   
         CLC   ACCSMETH,=C'ID'     DUMMY MEMBER WITHOUT INVENTORY?              
         BNE   *+14                                                             
         MVC   0(L'MRFRMEM,RE),=C'***NONE***'  YES: PUT IN TABLE ANYWAY         
         B     *+10                                                             
         MVC   0(L'MRFRMEM,RE),MRFRMEM   MEMBER NAME                            
         MVI   L'MRFRMEM(RE),C','                                               
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,L'PLIBCODE                                                    
         A     RE,=A(PLIBCODE)                                                  
         MVC   0(L'MRLIB,RE),MRLIB          LIBCODE                             
         MVC   L'MRLIB+1(L'MRSUB,RE),MRSUB  SUBCODE (COULD BE BLANKS)           
         CLC   MRSUB,SPACES                 ANY SUBCODE PRESENT?                
         BE    *+8                                                              
         MVI   L'MRLIB(RE),C'/'             YES: DELIMIT WITH SLASH             
         MVI   L'MRLIBC+1(RE),C','          COMMA GOES AFTER LIBC/SUB           
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,L'PTOUSRDT                                                    
         A     RE,=A(PTOUSRDT)                                                  
         MVI   0(RE),C'-'                                                       
         CLC   MRTODATA,SPACES                                                  
         BE    *+10                                                             
         MVC   0(L'MRTODATA,RE),MRTODATA    "TO" USER DATA                      
         MVI   L'PTOUSRDT(RE),C','                                              
*                                                                               
         CLC   ACCSMETH,=C'PV'     LIBCODE USES PANVALET ACCESS METHOD?         
         BNE   TYPE02W             NO                                           
*                                                                               
         LR    RE,R3               PROCESS PROD OUTPUT OBJECT NAMES             
         BCTR  RE,0                                                             
         MHI   RE,L'POBJECT                                                     
         A     RE,=A(POBJECT)                                                   
         MVI   0(RE),C'-'                                                       
         CLC   PPBPHASE,SPACES                                                  
         BE    *+10                                                             
         MVC   0(L'PPBPHASE,RE),PPBPHASE                                        
         MVI   L'PPBPHASE(RE),C','                                              
*                                                                               
         LR    RE,R3               PROCESS STGE PHASE NAMES                     
         BCTR  RE,0                                                             
         MHI   RE,L'PTOBJECT                                                    
         A     RE,=A(PTOBJECT)                                                  
         MVI   0(RE),C'-'                                                       
         CLC   MRLIB,=C'DICT'      LIBCODE DICT NEVER GETS STAGED               
         BE    TYPE02U                                                          
         CLC   MRSUB,=C'P  '       IS THIS A /P LIBCODE?                        
         BE    TYPE02U             YES: THESE AREN'T STAGED EITHER              
         CLC   PPTPHASE,SPACES                                                  
         BE    TYPE02U                                                          
         MVC   0(L'PPTPHASE,RE),PPTPHASE                                        
TYPE02U  MVI   L'PPTPHASE(RE),C','                                              
*                                                                               
         LR    RE,R3               PAN LEVEL NUMBERS                            
         BCTR  RE,0                                                             
         MHI   RE,L'PPLVL                                                       
         A     RE,=A(PPLVL)                                                     
         MVI   0(RE),C'-'                                                       
         CLC   PPLEVEL,SPACES                                                   
         BE    *+10                                                             
         MVC   0(L'PPLEVEL,RE),PPLEVEL                                          
         MVI   L'PPLEVEL(RE),C','                                               
*                                                                               
         LR    RE,R3               LAST MEMBER UPDATE USERID                    
         BCTR  RE,0                                                             
         MHI   RE,L'PPUSR                                                       
         A     RE,=A(PPUSR)                                                     
         MVI   0(RE),C'-'                                                       
         CLC   PPUSER,SPACES                                                    
         BE    *+10                                                             
         MVC   0(L'PPUSER,RE),PPUSER                                            
         MVI   L'PPUSER(RE),C','                                                
*                                                                               
         LR    RE,R3               PREVIOUS MR#S                                
         BCTR  RE,0                                                             
         MHI   RE,L'PREVMR#                                                     
         A     RE,=A(PREVMR#)                                                   
         MVI   0(RE),C'-'                                                       
         CLC   MEMRECMR,SPACES                                                  
         BE    *+10                                                             
         MVC   0(L'MEMRECMR,RE),MEMRECMR                                        
         MVI   L'MEMRECMR(RE),C','                                              
*                                                                               
         LR    RE,R3               PREVIOUS ITMF#                               
         BCTR  RE,0                                                             
         MHI   RE,L'PREVITMF                                                    
         A     RE,=A(PREVITMF)                                                  
         MVI   0(RE),C'-'                                                       
         CLC   MEMITMF#,SPACES                                                  
         BE    *+10                                                             
         MVC   0(L'MEMITMF#,RE),MEMITMF#                                        
         MVI   L'MEMITMF#(RE),C','                                              
*                                                                               
* FORMAT OF DATE/TIME IS: MM/DD/YYYY HH:MM:SS AM (OR PM)                        
*                                                                               
         LR    RE,R3               DATE/TIME OF LAST MEMBER UPDATE              
         BCTR  RE,0                                                             
         MHI   RE,L'PPDTTMU                                                     
         A     RE,=A(PPDTTMU)                                                   
         MVI   0(RE),C'-'                                                       
         CLC   PUDATE,SPACES                                                    
         BE    *+16                                                             
         MVC   0(L'PUDATE,RE),PUDATE                                            
         MVC   L'PUDATE+1(L'PUTIMEFM,RE),PUTIMEFM                               
         MVI   L'PUDATE+1+L'PUTIMEFM(RE),C','                                   
*                                                                               
TYPE02W  DS    0H                                                               
         CLOSE LIBCODES                                                         
*                                                                               
TYPE02X  DS    0H                                                               
         B     NEXT                                                             
         DROP  R6                                                               
         EJECT                                                                  
TYPE03   DS    0H                                                               
         LARL  R6,REC+4                                                         
         USING MRA,R6                                                           
         CLC   MRARECTP,=C'03'     APPROVAL RECORD?                             
         BNE   TYPE04              NO                                           
*                                                                               
         CLI   PARMACT,PROMOTE     IS THIS A PROMOTION?                         
         BNE   *+12                                                             
         MVI   BYTE,MRAPRMOV       YES: SET APPROVAL DIRECTION TO MOVE          
         B     *+16                                                             
         CLI   PARMACT,BACKOUT     NO: IS IT A BACKOUT?                         
         BNE   TYPE03X             NO: IGNORE APPROVAL RECORDS                  
         MVI   BYTE,MRAPRBKO       YES: SET APPROVAL DIRECTION TO BKOT          
*                                                                               
         CLC   PARMLVL,MRAPRLVL    CHECK MATCH ON APPROVAL LEVEL                
         BNE   TYPE03X                                                          
         CLC   BYTE,MRAPRDIR       CHECK MATCH ON APPROVAL DIRECTION            
         BNE   TYPE03X                                                          
*                                                                               
         MVC   PAPPRVR,MRAPRUID    APPROVER USERID                              
*                                                                               
         MVC   PPDATMON,MRAPRDTM   APPROVAL DATE                                
         MVI   PPDATMON+2,C'/'                                                  
         MVC   PPDATDAY,MRAPRDTD                                                
         MVI   PPDATDAY+2,C'/'                                                  
         MVC   PPDATYR,MRAPRDTC                                                 
*                                                                               
         MVC   PPTIMHR,MRAPRTMH    APPROVAL TIME                                
         MVI   PPTIMHR+2,C':'                                                   
         MVC   PPTIMMIN,MRAPRTMM                                                
         MVI   PPTIMMIN+2,C':'                                                  
         MVC   PPTIMSEC,MRAPRTMS                                                
*                                                                               
         LA    R4,PPDATE                                                        
         LA    R5,PPTIME                                                        
         BRAS  RE,REALTIME         CONVERT SYS DATE&TIME TO REAL TIME           
*                                                                               
TYPE03X  DS    0H                                                               
         B     NEXT                                                             
         DROP  R6                                                               
         EJECT                                                                  
TYPE04   DS    0H                                                               
         LARL  R6,REC+4                                                         
         USING MRV,R6                                                           
         CLC   MRVRECTP,=C'04'     VERIFICATION RECORD?                         
         BNE   TYPE04X             NO                                           
*                                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   TYPE04X             NO                                           
*                                                                               
         CLC   PARMLVL,MRVKVLVL    MATCH ON VERIFICATION LEVEL?                 
         BNE   TYPE04X             NO                                           
*                                                                               
* UNTIL THE NOTES AGENT CAN HANDLE VERIFICATION CATEGORIES OTHER THAN           
* THIS ONE, WE NEED TO IGNORE ANY OTHERS.                                       
* DEIS AUG/2019: WE MIGHT REVISIT THIS CODE IN THE FUTURE, NOW THAT             
* LOTUS NOTES IS NO LONGER IN THE PICTURE.                                      
         CLC   MRVKVCAT,=C'07'     CATEGORY 07 VERIFICATION?                    
         BNE   TYPE04X             NO: IGNORE IT                                
*                                                                               
         MVC   PACTION,=CL12'VERIFICATION'                                      
*                                                                               
         MVC   PADATMON,MRVDATMM   VERIFICATION DATE                            
         MVI   PADATMON+2,C'/'                                                  
         MVC   PADATDAY,MRVDATDD                                                
         MVI   PADATDAY+2,C'/'                                                  
         MVC   PADATYR,MRVDATCC                                                 
*                                                                               
         MVC   PATIMHR,MRVTIMHH    VERIFICATION TIME                            
         MVI   PATIMHR+2,C':'                                                   
         MVC   PATIMMIN,MRVTIMMM                                                
         MVI   PATIMMIN+2,C':'                                                  
         MVC   PATIMSEC,MRVDATSS                                                
*                                                                               
         LA    R4,PADATE                                                        
         LA    R5,PATIME                                                        
         BRAS  RE,REALTIME         CONVERT SYS DATE&TIME TO REAL TIME           
*                                                                               
TYPE04X  DS    0H                                                               
         B     NEXT                                                             
         DROP  R6                                                               
         EJECT                                                                  
CLOSE    DS    0H                                                               
         CLOSE MRFILE                                                           
         CLOSE UNLKCRDS                                                         
*                                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   WEREOKAY            NO, IT'S A SUBMIT                            
*                                                                               
         CLOSE LOCKCRDS                                                         
*&&DO*&& CLOSE NTFYGRPS                                                         
         CLOSE CURLOUT                                                          
         CLOSE CRDS5955                                                         
         CLOSE LOADMODS                                                         
*                                                                               
         CLC   RETCODE,=F'8'       FATAL ERROR FOUND?                           
         BE    EXIT                                                             
*                                                                               
         BRAS  RE,CHKDUPS          CHECK FOR DUPLICATE LIVE PHASE NAMES         
         BE    CHKLVLS             THERE ARE NONE                               
*                                                                               
         MVC   RETCODE,=F'8'       SET RC=8 (VERIFICATION FAILURE)              
         MVC   MESSAGE1,MSG07_1    DUPLICATE LIVE PHASE NAMES                   
         MVC   MESSAGE2,MSG07_2                                                 
         BRAS  RE,SNDMAIL                                                       
         B     EXIT                NO NEED TO DO ANYTHING ELSE                  
*                                                                               
CHKLVLS  DS    0H                                                               
*                                                                               
* CHECK THE LEVEL TABLE TO COMPARE THE REQUIRED LEVEL HISTORY WITH THE          
* ACTUAL MR PROMOTION HISTORY, TO DETERMINE WHETHER THE CURRENT MR              
* STATUS IS CORRECT. IF IT IS NOT, WE FORCE THE MR VERIFICATION TO BE           
* UNSUCCESSFUL, AND WE PUSH AN E-MAIL TELLING THE PROGRAMMER TO CHANGE          
* THE MR STATUS MANUALLY AND REISSUE THE RVP.                                   
*                                                                               
         MVC   FULL,SPACES         WILL HOLD "SHOULD BE" STATUS                 
         MVI   FULL,C'A'           STATUS WILL ALWAYS START WITH "A"            
         LA    R5,LEVELTAB         TABLE OF PANAPT LEVELS                       
         USING LEVELTBD,R5                                                      
*                                                                               
NXTLEVEL DS    0H                                                               
         CLI   LVLREQD,C'Y'        IS THIS LEVEL REQUIRED?                      
         BNE   BUMPLVL             NO: BUMP TO NEXT LEVEL                       
         CLC   LVLDATEM,SPACES     YES: DID WE PROMOTE TO THIS LEVEL?           
         BNE   BUMPLVL             YES: BUMP TO NEXT LEVEL                      
*                                                                               
         MVC   FULL+2(2),LVLABBRV  NO: THIS IS THE TARGET LEVEL                 
         MVI   FULL+1,C'W'         ASSUME STATUS IS "AWAITING"                  
         CLI   LVLAVREQ,C'Y'       RVP(S) OR APPROVAL(S) REQUIRED?              
         BE    *+8                                                              
         MVI   FULL+1,C'P'         NO: IT'S "APPROVED" FOR THAT LEVEL           
         B     CHKSTAT                                                          
*                                                                               
BUMPLVL  DS    0H                                                               
         LA    R5,LEVELTBQ(R5)     NO: BUMP TO NEXT LEVEL                       
         B     NXTLEVEL                                                         
*                                                                               
CHKSTAT  DS    0H                                                               
         CLC   LVLSPS,LEVELPOS     IF "CORRECT" LEVEL IS HIGHER THAN            
         BNL   WEREOKAY             CURRENT MR LEVEL, THEN THE RVP IS           
*                                   OKAY. PANAPT WILL EVENTUALLY CHANGE         
*                                   THE MR STATUS ON THE "SUB".                 
*                                                                               
         CLC   FULL,PSTA           IS CURRENT MR STATUS CORRECT?                
         BE    WEREOKAY            YES                                          
*                                                                               
         MVC   RETCODE,=F'8'       SET RC=8: VERIFICATION FAILURE               
         MVC   MESSAGE1,MSG08_1    INVALID MOVE REQUEST STATUS                  
         MVC   MESSAGE2,MSG08_2A                                                
         MVC   MESSAGE2+51(4),LVLSHORT  SHORT LEVEL NAME                        
         CLI   FULL+1,C'W'         ASSUME STATUS IS "AWAITING"                  
         BE    *+16                CORRECT                                      
         MVC   MESSAGE2,MSG08_2B                                                
         MVC   MESSAGE2+55(4),LVLSHORT  SHORT LEVEL NAME                        
         DROP  R5                                                               
*                                                                               
         BRAS  RE,SNDMAIL                                                       
         B     EXIT                NO NEED TO DO ANYTHING ELSE                  
*                                                                               
WEREOKAY DS    0H                                                               
         IF (CLI,PUPDMTAB,EQ,C'Y'),AND, WAS MEMBER INFO TABLE BUILT?            
            (CLI,PARMACT,EQ,PROMOTE)    ...AND IS THIS A MR PROMOTION?          
           BRAS  RE,GENMEMTAB           YES: GENERATE JIRA MEMBER TABLE         
         ENDIF ,                                                                
*                                                                               
EXIT     DS    0H                                                               
         CLI   MAILSENT,C'Y'       DID WE SEND ANY E-MAILS?                     
         BNE   XBASE               NO                                           
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)  YES: DETACH JESMAIL                
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE                                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
MSG02_1  DC    CL80'Verification Failure! All other required Verificati+        
               ons must be satisfied first.'                                    
MSG02_2  DC    CL80'The Category 07 Verification must always be the las+        
               t one obtained.'                                                 
MSG03_1  DC    CL80'Verification Failure! Move Requests with Move Type +        
               "T" (Temporary) cannot be'                                       
MSG03_2  DC    CL80'promoted beyond STGE. (The Move Type may be changed+        
                via PanAPT "CHG" action.)'                                      
MSG04_1  DC    CL80'Verification failure! The Libcodes used in this Mov+        
               e Request are inconsistent'                                      
MSG04_2  DC    CL80'with each other.'                                           
MSG05_1  DC    CL80'Verification failure! "A" or "C" flag found on memb+        
               er(s) in this Move Request.'                                     
MSG05_2  DC    CL80'Use the PanAPT "CHG" action to see and correct the +        
               problem member(s).'                                              
MSG06_1  DC    CL80'Verification failure! Member MMMMMMMMMM not found i+        
               n'                                                               
MSG07_1  DC    CL80'Verification failure! Duplicate live phase names ex+        
               ist in this Move Request.'                                       
MSG07_2  DC    CL80'This is invalid even if only promoting to STGE. Use+        
                multiple MRs if necessary.'                                     
MSG08_1  DC    CL80'This MR has an invalid Status (most likely due to a+        
                member Libcode change). Use'                                    
MSG08_2A DC    CL80'the STA(tus) command to status the MR to "Awaiting +        
               XXXX App", then redo the RVP.'                                   
MSG08_2B DC    CL80'the STA(tus) command to status the MR to "Approved +        
               for XXXX", then redo the RVP.'                                   
MSG09_1  DC    CL80'The DDPAPTMRM (Verification) program supports a max+        
               imum of NNNN members in a'                                       
MSG09_2  DC    CL80'single Move Request. Break up the MR, or speak with+        
                your PanAPT Administrator.'                                     
         SPACE 2                                                                
         ORG   PAPTMRM+(((*-PAPTMRM)/4096)+1)*4096                              
         SPACE 2                                                                
         DROP  RB,R9                                                            
         EJECT                                                                  
GENLOCK  NTR1  BASE=(*,GENLOCKXX),LABEL=*                                       
*                                                                               
* GENERATE PANVALET LIBRARY LOCK CONTROL CARD(S).                               
*                                                                               
* WE NEED TO LOCK EACH PANVALET MEMBER ASSOCIATED WITH THE MR ON ITS            
* ORIGIN PANVALET LIBRARY WHEN WE DO THE FIRST RVP. THAT RVP MAY WELL           
* BE TO STGE, AND NOT ALL LIBCODES INCLUDE THE STGE LEVEL. SO WE BUILD          
* SPECIAL CONTROL CARDS AS INPUT TO A REXX ROUTINE WHICH RUNS AT THE            
* END OF THE RVP PROCESS AND LOCKS ALL THE PAN MEMBERS. THESE CONTROL           
* CARDS CONTAIN THE MEMBER NAME IN COLUMN 1, FOLLOWED BY AT LEAST ONE           
* BLANK, FOLLOWED BY THE DSN OF ITS ORIGIN PAN LIBRARY.                         
*                                                                               
* NOTE: R6 IS ASSUMED TO BE POINTING TO THE PANAPT "TYPE 02" MEMBER             
*       RECORD.                                                                 
*                                                                               
         USING MMMBRD,R6                                                        
*                                                                               
         MVC   RECORD,SPACES       BUILD LOCK CARD FOR SOURCE MEMBER            
         MVC   RECORD(10),MRFRMEM  MOVE MEMBER NAME INTO LOCK CARD              
         MVC   RECORD+11(44),ORIGDSN  ORIGIN PAN LIBRARY DSN                    
         PUT   LOCKCRDS,RECORD                                                  
*                                                                               
         CLC   =C'ACAT',MRLIB      ACAT MEMBERS GENERATE A RELO                 
         BNE   GENLOC10                                                         
*                                                                               
         MVC   RECORD,SPACES       BUILD LOCK CARD FOR RELO MEMBER              
         MVC   RECORD(10),OUTNAME  MOVE MEMBER NAME INTO LOCK CARD              
         MVC   RECORD+11(44),RELATDSN  RELATED LIBCODE DSN                      
         PUT   LOCKCRDS,RECORD                                                  
         B     GENLOCKX                                                         
*                                                                               
GENLOC10 DS    0H                                                               
         CLC   =C'GEN',MRLIB       GEN MEMBERS GENERATE SCREEN DSECT            
         BNE   GENLOCKX                                                         
*                                                                               
         MVC   RECORD,SPACES       BUILD LOCK CARD FOR SCREEN DSECT             
         MVC   RECORD(10),MRFRMEM  MOVE MEMBER NAME INTO LOCK CARD              
         LA    RE,RECORD+9         A(END OF LONGEST POSS. MEMBER NAME)          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)            POINT PAST END OF MEMBER NAME                
         MVI   0(RE),C'D'          SUFFIX MEMBER NAME WITH 'D'                  
         MVC   RECORD+11(44),RELATDSN  RELATED LIBCODE DSN                      
         PUT   LOCKCRDS,RECORD                                                  
*                                                                               
GENLOCKX DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
GENLOCKXX EQU  *                                                                
         EJECT                                                                  
GENUNLK  NTR1  BASE=(*,GENUNLKXX),LABEL=*                                       
*                                                                               
* GENERATE PANVALET LIBRARY UNLOCK CONTROL CARD(S).                             
*                                                                               
* GENERATE THE ++UNLOCK CARDS TO UNLOCK THE MEMBERS IN THE EVENT OF A           
* STGE BACKOUT. THIS IS NECESSARY BECAUSE NOT ALL LIBCODES HAVE THE             
* STGE LEVEL DEFINED, SO THE PANAPT MODELS DON'T GET A HOOK TO PROCESS          
* EACH MEMBER.                                                                  
*                                                                               
* NOTE: R6 IS ASSUMED TO BE POINTING TO THE PANAPT "TYPE 02" MEMBER             
*       RECORD.                                                                 
*                                                                               
         USING MMMBRD,R6                                                        
*                                                                               
         MVC   RECORD,SPACES       BUILD %+UNLOCK CARD FOR SOURCE MEM.          
         MVC   RECORD(8),=C'%+UNLOCK'                                           
         MVC   RECORD+9(10),MRFRMEM  MOVE MEMBER NAME INTO %+UNLOCK CRD         
         LA    RE,RECORD+18        A(END OF LONGEST POSS. MEMBER NAME)          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)            POINT PAST END OF MEMBER NAME                
         MVC   0(8,RE),=C',OLDID=*'                                             
         PUT   UNLKCRDS,RECORD                                                  
*                                                                               
         CLC   =C'ACAT',MRLIB      ACAT MEMBERS GENERATE A RELO                 
         BNE   GENUNL10                                                         
*                                                                               
         MVC   RECORD,SPACES       BUILD %+UNLOCK CARD FOR RELO MEMBER          
         MVC   RECORD(8),=C'%+UNLOCK'                                           
         MVC   RECORD+9(10),OUTNAME  MOVE MEMBER NAME INTO %+UNLOCK CRD         
         LA    RE,RECORD+18        A(END OF LONGEST POSS. MEMBER NAME)          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)            POINT PAST END OF MEMBER NAME                
         MVC   0(8,RE),=C',OLDID=*'                                             
         PUT   UNLKCRDS,RECORD                                                  
         B     GENUNLKX                                                         
*                                                                               
GENUNL10 DS    0H                                                               
         CLC   =C'GEN',MRLIB       GEN MEMBERS GENERATE SCREEN DSECT            
         BNE   GENUNLKX                                                         
*                                                                               
         MVC   RECORD,SPACES       BUILD %+UNLOCK CARD FOR SCREEN DSECT         
         MVC   RECORD(8),=C'%+UNLOCK'                                           
         MVC   RECORD+9(10),MRFRMEM  MOVE MEMBER NAME INTO %+UNLOCK CRD         
         LA    RE,RECORD+18        A(END OF LONGEST POSS. MEMBER NAME)          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)            POINT PAST END OF MEMBER NAME                
         MVC   0(9,RE),=C'D,OLDID=*' SUFFIX MEMBER NAME WITH 'D'                
         PUT   UNLKCRDS,RECORD                                                  
*                                                                               
GENUNLKX DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
GENUNLKXX EQU  *                                                                
         EJECT                                                                  
CHKDUPS  NTR1  BASE=(*,CHKDUPSX),LABEL=*                                        
*                                                                               
* WE NEED TO MAKE SURE THAT THERE ARE NO DUPLICATE BASE PHASE NAMES             
* PRESENT IN THE MOVE REQUEST. FOR EXAMPLE, IF A GIVEN MOVE REQUEST             
* INCLUDES TWO PHASES WITH PHASE NAMES "XYZA" AND "XYZB", THEN THESE            
* WILL BOTH RESULT IN THE SAME "@-SUFFIXED" PHASE NAME OF "XYZ@". THAT          
* COLLISION WOULD CAUSE THE LOAD MODULE PROMOTION JOB TO FAIL WHEN              
* DOING THE "COPY WITH RENAME" OPERATION FROM TESTLIB TO LOADLIB.               
*                                                                               
* WE CHECK FOR DUPS BY COPYING POBJECT (THE ARRAY OF LIVE OUTPUT NAMES)         
* INTO A WORK AREA (BLOCK) WHICH WE THEN SORT BY OUTPUT OBJECT NAME.            
* HOWEVER, NOT ALL OUTPUT OBJECT NAMES ARE PHASE NAMES (E.G., AN OUTPUT         
* OBJECT COULD BE A RELO). SO WE EXAMINE THE MEMBER'S LIBCODE TO SEE            
* WHETHER OR NOT WE NEED TO PERFORM THE DUPLICATE CHECK ON IT.                  
*                                                                               
         LARL  R0,BLOCK            WORK AREA                                    
         LARL  RE,POBJECT          ARRAY OF OUTPUT OBJECT NAMES                 
         LHI   R1,POBJECTLQ        L'ARRAY                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY THE ARRAY                               
         BNO   *+6                                                              
         DC    H'0'                DESTRUCTIVE MOVE ?!?                         
*                                                                               
         LARL  RE,PLIBCODE         ARRAY OF LIBCODES                            
         LARL  RF,BLOCK            ARRAY OF LIVE OUTPUT OBJECT NAMES            
         LHI   R0,MAXMEMQ          MAX. NO. OF MEMBER TABLE ENTRIES             
*                                                                               
CHKDUP10 DS    0H                                                               
         CLC   =C'ALNK',0(RE)      DOES THE LIBCODE INVOLVE A PHASE?            
         BE    CHKDUP15                                                         
         CLC   =C'APG',0(RE)                                                    
         BE    CHKDUP15                                                         
         CLC   =C'DPG',0(RE)                                                    
         BE    CHKDUP15                                                         
         CLC   =C'GEN',0(RE)                                                    
         BE    CHKDUP15                                                         
         CLC   =C'LNK',0(RE)                                                    
         BE    CHKDUP15                                                         
         CLC   =C'RRG',0(RE)                                                    
         BE    CHKDUP15                                                         
*                                                                               
         MVI   0(RF),X'FF'         NO: REMOVE THIS OBJECT NAME                  
*                                                                               
CHKDUP15 DS    0H                                                               
         LA    RF,L'POBJECT(RF)                                                 
         LA    RE,L'PLIBCODE(RE)                                                
         BCT   R0,CHKDUP10                                                      
*                                                                               
* SORT THE ENTIRE ARRAY. THE REMOVED NAMES WILL SORT AT THE END                 
* (BECAUSE THE HOB OF THE REMOVED NAMES WERE OVERWRITTEN BY A HIGH              
* VALUE OF X'FF' JUST ABOVE).                                                   
*                                                                               
         LHI   R0,MAXMEMQ          MAX. NO. OF MEMBER TABLE ENTRIES             
         GOTO1 =V(XSORT),DMCB,A(BLOCK),(R0),L'POBJECT,L'POBJECT,0               
*                                                                               
         LARL  RF,BLOCK            SORTED ARRAY OF LIVE PHASE NAMES             
CHKDUP20 CLI   0(RF),X'FF'         END OF LIST?                                 
         BE    YES                                                              
         CLC   0(L'POBJECT,RF),L'POBJECT(RF)                                    
         BE    NO                  DUPLICATE LIVE PHASE NAME FOUND              
         LA    RF,L'POBJECT(RF)                                                 
         BCT   R0,CHKDUP20                                                      
         B     YES                 ENTIRE ARRAY IS FULL AND OKAY                
*                                                                               
         ANSR                                                                   
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
CHKDUPSX EQU   *                                                                
         EJECT                                                                  
GENMEMTAB NTR1 BASE=(*,GENMEMTABX),LABEL=*                                      
*                                                                               
* GENERATE A MEMBER TABLE FOR JIRA                                              
*  THE TABLE IS GENERATED IN TWO FORMATS:                                       
*    1. PLAIN TEXT, INTENDED FOR INCLUSION IN AN ISSUE *ATTACHMENT*.            
*    2. JSON, IN A FORMAT DOCUMENTED AS A JIRA ISSUE *COMMENT*.                 
*                                                                               
         OPEN  (MEMTAB,OUTPUT)     FOR ATTACHMENT                               
         OPEN  (MEMTABJS,OUTPUT)   FOR COMMENT                                  
*                                                                               
         LTR   R6,R3               NUMBER OF MEMBERS IN MEMBER TABLE            
         JZ    GENMEMX             (SHOULD REALLY NEVER BE ZERO!)               
*                                                                               
         LA    R2,WORK                                                          
         USING PRINT_MEMBER_TABLE,R2                                            
*                                                                               
         SR    R5,R5               R5: TABLE INDEX                              
*                                                                               
         MVI   WORK,C' '           MEMBER TABLE HEADLINE ROW 1                  
         MVC   WORK+1(L'WORK-1),WORK  INIT OUTPUT RECORD WITH SPACES            
         MVC   PRINT_MEMBER_NAME,=C'Panvalet  '                                 
         MVC   PRINT_LIBCODE,=C'PanAPT  '                                       
         MVC   PRINT_TO_USER_DATA,=C'PanAPT  '                                  
         MVC   PRINT_PAN_LEVEL,=C'Lvl'                                          
         MVC   PRINT_PAN_UPDATED,=C'Last Source Update    '                     
         MVC   PRINT_PAN_USERID,=C'Updater '                                    
         MVC   PRINT_OBJECT_NAME,=C'Output    '                                 
         MVC   PRINT_STAGE_OBJECT,=C'Stage     '                                
         MVC   PRINT_PREVIOUS_MR#,=C'Prev. '                                    
         MVC   PRINT_PREVIOUS_ITMF#,=C'Prev.           '                        
         PUT   MEMTAB,WORK                                                      
*                                                                               
         MVI   WORK,C' '           MEMBER TABLE HEADLINE ROW 2                  
         MVC   WORK+1(L'WORK-1),WORK  INIT OUTPUT RECORD WITH SPACES            
         MVC   PRINT_MEMBER_NAME,=C'Member    '                                 
         MVC   PRINT_LIBCODE,=C'Libcode '                                       
         MVC   PRINT_TO_USER_DATA,=C'UserData'                                  
         MVC   PRINT_PAN_LEVEL,=C'No.'                                          
         MVC   PRINT_PAN_UPDATED,=C'Date/Time             '                     
         MVC   PRINT_PAN_USERID,=C'Userid  '                                    
         MVC   PRINT_OBJECT_NAME,=C'Object    '                                 
         MVC   PRINT_STAGE_OBJECT,=C'Object    '                                
         MVC   PRINT_PREVIOUS_MR#,=C'MR#   '                                    
         MVC   PRINT_PREVIOUS_ITMF#,=C'ITMF#           '                        
         PUT   MEMTAB,WORK                                                      
*                                                                               
         MVI   WORK,C' '           MEMBER TABLE HEADLINE ROW 3                  
         MVC   WORK+1(L'WORK-1),WORK  INIT OUTPUT RECORD WITH SPACES            
         MVC   PRINT_MEMBER_NAME,=C'----------'                                 
         MVC   PRINT_LIBCODE,=C'--------'                                       
         MVC   PRINT_TO_USER_DATA,=C'--------'                                  
         MVC   PRINT_PAN_LEVEL,=C'---'                                          
         MVC   PRINT_PAN_UPDATED,=C'----------------------'                     
         MVC   PRINT_PAN_USERID,=C'--------'                                    
         MVC   PRINT_OBJECT_NAME,=C'----------'                                 
         MVC   PRINT_STAGE_OBJECT,=C'----------'                                
         MVC   PRINT_PREVIOUS_MR#,=C'------'                                    
         MVC   PRINT_PREVIOUS_ITMF#,=C'----------------'                        
         PUT   MEMTAB,WORK                                                      
*                                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(8),=C'{"body":'                                            
         PUT   MEMTABJS,WORK2      PUT "body" TAG                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(27),=C'"h3.Panvalet member table\n'                        
         PUT   MEMTABJS,WORK2      PUT "h3"-STYLE COMMENT HEADER                
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(124),=C'||Member||Libcode||UserData||Level||Last S+        
               ource Update||Userid||Output object||Stage object||Prev.+        
                MR#||Prev. ITMF Issue||\n'                                      
         PUT   MEMTABJS,WORK2      PUT TABLE COLUMN HEADINGS                    
*                                                                               
GENMEM10 DS    0H                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK  INIT OUTPUT RECORD WITH SPACES            
*                                                                               
         MVC   WORK2,SPACES                                                     
         LA    R3,WORK2            BUILD A TABLE ROW HERE                       
*                                                                               
         LARL  R4,PMEMBER          A(MEMBER NAME ARRAY)                         
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PMEMBER        L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_MEMBER_NAME,0(R4)                                          
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PMEMBER-1,R3),0(R4)                                          
         LA    R3,L'PMEMBER(,R3)                                                
*                                                                               
         LARL  R4,PLIBCODE         A(MEMBER NAME ARRAY)                         
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PLIBCODE       L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_LIBCODE,0(R4)                                              
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PLIBCODE-1,R3),0(R4)                                         
         LA    R3,L'PLIBCODE(,R3)                                               
*                                                                               
         LARL  R4,PTOUSRDT         A(MEMBER NAME ARRAY)                         
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PTOUSRDT       L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_TO_USER_DATA,0(R4)                                         
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PTOUSRDT-1,R3),0(R4)                                         
         LA    R3,L'PTOUSRDT(,R3)                                               
*                                                                               
         LARL  R4,PPLVL            A(PAN LEVEL NUMBER)                          
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PPLVL          L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_PAN_LEVEL,0(R4)                                            
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PPLVL-1,R3),0(R4)                                            
         LA    R3,L'PPLVL(,R3)                                                  
*                                                                               
         LARL  R4,PPDTTMU          A(USERID)                                    
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PPDTTMU        L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_PAN_UPDATED,0(R4)                                          
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PPDTTMU-1,R3),0(R4)                                          
         LA    R3,L'PPDTTMU(,R3)                                                
*                                                                               
         LARL  R4,PPUSR            A(USERID)                                    
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PPUSR          L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_PAN_USERID,0(R4)                                           
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PPUSR-1,R3),0(R4)                                            
         LA    R3,L'PPUSR(,R3)                                                  
*                                                                               
         LARL  R4,POBJECT          A(OUTPUT OBJECT NAME ARRAY)                  
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'POBJECT        L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_OBJECT_NAME,0(R4)                                          
         MVI   0(R3),C'|'                                                       
         MVC   1(L'POBJECT-1,R3),0(R4)                                          
         LA    R3,L'POBJECT(,R3)                                                
*                                                                               
         LARL  R4,PTOBJECT         A(STAGE OBJECT NAME ARRAY)                   
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PTOBJECT       L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_STAGE_OBJECT,0(R4)                                         
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PTOBJECT-1,R3),0(R4)                                         
         LA    R3,L'PTOBJECT(,R3)                                               
*                                                                               
         LARL  R4,PREVMR#          A(PREVIOUS MR# ARRAY)                        
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PREVMR#        L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_PREVIOUS_MR#,0(R4)                                         
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PREVMR#-1,R3),0(R4)                                          
         LA    R3,L'PREVMR#(,R3)                                                
*                                                                               
         LARL  R4,PREVITMF         A(PREVIOUS ITMF# ARRAY)                      
         LR    R0,R5               CURRENT INDEX VALUE                          
         MHI   R0,L'PREVITMF       L'ELEMENT                                    
         AR    R4,R0               R4 = A(CURRENT ENTRY)                        
         MVC   PRINT_PREVIOUS_ITMF#,0(R4)                                       
         MVI   0(R3),C'|'                                                       
         MVC   1(L'PREVITMF-1,R3),0(R4)                                         
         LA    R3,L'PREVITMF(,R3)                                               
*                                                                               
         IF (CHI,R6,NE,1)          LAST TABLE ENTRY?                            
           MVC  0(3,R3),=C'|\n'      NO: FORCE NEWLINE                          
         ELSE  ,                                                                
           MVC  0(3,R3),=C'|"}'      YES: CLOSE THE "body" TAG                  
         ENDIF ,                                                                
*                                                                               
         PUT   MEMTAB,WORK         PUT TABLE ROW TO ATTACHMENT FILE             
         PUT   MEMTABJS,WORK2      PUT TABLE ROW TO COMMENT FILE                
*                                                                               
         AHI   R5,1                BUMP TO NEXT MEMBER                          
*                                                                               
         BCT   R6,GENMEM10                                                      
*                                                                               
GENMEMX  DS    0H                                                               
         CLOSE MEMTAB                                                           
         CLOSE MEMTABJS                                                         
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
GENMEMTABX EQU *                                                                
         SPACE 2                                                                
PRINT_MEMBER_TABLE DSECT                                                        
                   DS    CL1       COL. 1 IS BLANK (SEE MODEL DDJMCRPT)         
PRINT_MEMBER_NAME  DS    CL(L'MRFRMEM)                                          
                   DS    CL2                                                    
PRINT_LIBCODE      DS    CL(L'MRLIBC+1)   (ADD 1 FOR SLASH)                     
                   DS    CL2                                                    
PRINT_TO_USER_DATA DS    CL(L'MRTODATA)                                         
                   DS    CL2                                                    
PRINT_PAN_LEVEL    DS    CL(L'PPLEVEL)                                          
                   DS    CL2                                                    
PRINT_PAN_UPDATED  DS    CL(L'PUDATE+1+L'PUTIMEFM)                              
                   DS    CL2                                                    
PRINT_PAN_USERID   DS    CL(L'PPUSER)                                           
                   DS    CL2                                                    
PRINT_OBJECT_NAME  DS    CL10                                                   
                   DS    CL2                                                    
PRINT_STAGE_OBJECT DS    CL10                                                   
                   DS    CL2                                                    
PRINT_PREVIOUS_MR# DS    CL(L'MEMRECMR)                                         
                   DS    CL2                                                    
PRINT_PREVIOUS_ITMF# DS  CL(L'MEMITMF#)                                         
*                                                                               
PAPTMRM  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT THE SYSTEM DATE AND TIME TO REAL DATE AND TIME IN THE US              
*  (DDS CLOCK TIME IS 6AM REAL TIME IN THE US ONLY)                             
* INPUT:  R4 - A(DATE)  MM/DD/YYYY                                              
*         R5 - A(TIME)  HH:MM:SS                                                
* OUTPUT: WILL OVERRIDE WHAT IS IN DATE AND TIME                                
***********************************************************************         
REALTIME NTR1  BASE=(*,REALTIMX),LABEL=*                                        
*                                                                               
*&&UK*&& B     RTX                 NO ADJUSTMENT NEEDED IN UK                   
*                                                                               
         CLC   0(2,R5),=C'18'      TIME PASSED MIDNIGHT?                        
         BL    RT50                NO - TIME+6HR AND EXIT                       
*                                                                               
         MVC   WORK(2),8(R4)       YY                                           
         MVC   WORK+2(2),0(R4)     YYMM                                         
         MVC   WORK+4(2),3(R4)     YYMMDD                                       
*                                  ADD 1 DAY                                    
         GOTO1 =V(ADDAY),DMCB,(C'D',WORK),(X'20',WORK),1                        
*                                  OVERRIDE THE INPUT DATE                      
         MVC   8(2,R4),WORK        YY                                           
         MVC   0(2,R4),WORK+2      MM                                           
         MVC   3(2,R4),WORK+4      DD                                           
*                                                                               
         CLC   0(2,R5),=C'20'      HH < 20?                                     
         BNL   RT30                NO                                           
*                                  MINUS 18 HR                                  
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)          HH                                           
         SHI   R1,X'0108'          F1F? - 0108                                  
         STCM  R1,3,0(R5)          OVERRIDE THE TIME (HH)                       
         B     RTX                                                              
*                                                                               
RT30     EQU   *                   HH >= 20                                     
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)          HH                                           
         AHI   R1,X'0002'          F2F? + 0002, EX:  20 -> 22                   
         SHI   R1,X'0200'          F2F? - 0200, THEN 22 -> 02                   
         STCM  R1,3,0(R5)                                                       
         B     RTX                                                              
*                                                                               
RT50     EQU   *                   ADD 6 HR TO THE TIME                         
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)          HH                                           
*                                                                               
         CLI   1(R5),C'4'          CARRY OVER?                                  
         BNL   *+12                YES                                          
         AHI   R1,X'0006'          F?F? + 0006                                  
         B     RT60                                                             
*                                                                               
         AHI   R1,X'0100'          ADD CARRY OVER                               
         SHI   R1,X'0004'          F1F? - 0004, THEN 4 -> 0                     
*                                                                               
RT60     STCM  R1,3,0(R5)          OVERRIDE THE TIME (HH)                       
*                                                                               
RTX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         DROP  RB                                                               
         LTORG                                                                  
REALTIMX EQU   *                                                                
         EJECT                                                                  
GETPHASE NTR1  BASE=(*,GETPHASX),LABEL=*                                        
         LARL  R6,REC+4                                                         
         USING MMMBRD,R6                                                        
*                                                                               
         MVC   TSTNAME,SPACES      TEST OUTPUT OBJECT NAME                      
         MVC   OUTNAME,SPACES      PRODUCTION OUTPUT OBJECT NAME                
         MVI   ERRFLAG,C'N'        ASSUME NO ERROR WILL BE FOUND                
*                                                                               
         LA    R5,RECORD           0-UP DIRECTORY ENTRY                         
         MVC   OUTLEVEL,DLEVEL-DIRENTRY(R5)     LEVEL NUMBER                    
         LARL  R5,AUDITINF                                                      
         USING AUDITD,R5                                                        
*                                                                               
* NOTE: CA MUST HAVE ADDED THE "LAST UPDATE DATE/TIME" TO THE AUDIT             
*       DATA AREA AT SOME POINT *AFTER* DDS BEGAN USING PANVALET.               
*       I.E., A VERY OLD PAN MEMBER MIGHT NOT HAVE THIS INFORMATION IN          
*       THE AUDIT AREA. SO WE NE NEED TO ALLOW FOR THAT POSSIBILITY.            
*                                                                               
         MVC   OUTUSER,LUPDUID     USER WHO LAST UPDATED MBR (OR BLANK)         
         MVC   DUB,LUPDDAT         MM/DD/YY FORMAT * EVEN IN THE UK! *          
*&&UK                                                                           
         MVC   HALF,DUB+3          FORCE THIS DATE TO THE STANDARD UK           
         MVC   DUB+3(2),DUB         FORMAT: DD/MM/YY                            
         MVC   DUB(2),HALF                                                      
*&&                                                                             
         IF (CLC,DUB,NE,SPACES)    IF "LAST UPDATE DATE" IS PRESENT:            
           GOTO1 =V(DATCON),DMCB,(4,DUB),(20,OUTDATEU) MAKE IT YYYYMMDD         
         ELSE ,                    O/W:                                         
           MVC  OUTDATEU,DUB         IT'S VERY OLD. LEAVE IT BLANK.             
         ENDIF ,                                                                
         MVC   OUTTIMEU,LUPDTIM    LAST UPDATE TIME (COULD BE SPACES)           
         DROP  R5                                                               
*                                                                               
         CLC   =C'ACAT',MRLIB                                                   
         BE    GPACAT                                                           
         CLC   =C'ALNK',MRLIB                                                   
         BE    GPALNK                                                           
         CLC   =C'LNK',MRLIB                                                    
         BE    GPLNK                                                            
         CLC   =C'INCL',MRLIB                                                   
         BE    GPX                                                              
         CLC   =C'APPLDEL',MRLIBC                                               
         BE    GPX                                                              
         CLC   =C'SRCE',MRLIB                                                   
         BE    GPSRCE                                                           
         CLC   =C'GEN',MRLIB                                                    
         BE    GPGEN                                                            
         CLC   =C'APG',MRLIB                                                    
         BE    GPAPG                                                            
         CLC   =C'RRG',MRLIB                                                    
         BE    GPRRG                                                            
         CLC   =C'DPG',MRLIB                                                    
         BE    GPDPG                                                            
         CLC   =C'MACR',MRLIB                                                   
         BE    GPMACR                                                           
         CLC   =C'DICT',MRLIB                                                   
         BE    GPX                 ***NEED SOME CODE FOR IT****                 
         DC    H'0'                UNKNOWN LIBCODE                              
*                                                                               
GPACAT   GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPACAT10            YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPACAT10 DS    0H                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   GPACAT20            NO: DON'T LOOK FOR "SRCE" FLOWER BOX         
*                                                                               
         CLC   =C'*$PANAPT01S$',RECORD  CHECK FOR EYE-CATCHER                   
         BNE   GPACAT20            "SRCE" LIBCODE FLOWER BOX PRESENT?           
         MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIFICATION FAILURE)         
         MVC   MESSAGE1,MSG10_1    SRCE LIBCODE COMMENT BLOCK PRESENT           
         MVC   MESSAGE1+67(L'MRFRMEM),MRFRMEM  MEMBER NAME                      
         MVC   MESSAGE2,MSG10_2                                                 
         MVI   ERRFLAG,C'Y'        FATAL ERROR                                  
*                                                                               
GPACAT20 DS    0H                                                               
         CLC   =C'*CATALP ',RECORD IS THIS THE *CATALP CARD?                    
         BNE   GPACAT              NO: KEEP READING                             
*                                                                               
         LA    R5,RECORD+8                                                      
         CLI   0(R5),C' '          START OF NAME?                               
         BNE   *+12                YES, EXTRACT IT                              
         LA    R5,1(R5)            NO, ADVANCE POINTER                          
         B     *-12                                                             
*                                                                               
         TRT   0(9,R5),TRTBLANK    FIND BLANK OR COMMA                          
         SR    R1,R5               LENGTH OF NAME                               
         MVC   OUTNAME(2),=CL2'RM' RM PREFIX FOR OBJECT                         
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTNAME+2(0),0(R5)                                               
         B     GPX                                                              
         SPACE 3                                                                
GPMACR   DS    0H                                                               
         MVC   OUTNAME(8),MRFRMEM+2 STRIP OFF "MC" FROM PAN MEMBER NAME         
         B     GPX                                                              
         SPACE 3                                                                
GPALNK   DS    0H                                                               
GPLNK    DS    0H                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPALNK10            YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPALNKX                                                          
*                                                                               
GPALNK10 DS    0H                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   GPALNK15            NO: DON'T LOOK FOR "SRCE" FLOWER BOX         
*                                                                               
         CLC   =C'*$PANAPT01S$',RECORD  CHECK FOR EYE-CATCHER                   
         BNE   GPALNK15            "SRCE" LIBCODE FLOWER BOX PRESENT?           
         MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIFICATION FAILURE)         
         MVC   MESSAGE1,MSG10_1    SRCE LIBCODE COMMENT BLOCK PRESENT           
         MVC   MESSAGE1+67(L'MRFRMEM),MRFRMEM  MEMBER NAME                      
         MVC   MESSAGE2,MSG10_2                                                 
         MVI   ERRFLAG,C'Y'        FATAL ERROR                                  
*                                                                               
GPALNK15 DS    0H                                                               
         CLC   =C'*PHASE ',RECORD  IS THIS THE *PHASE CARD?                     
         BNE   GPLNK               NO: KEEP READING                             
*                                                                               
         LA    R5,RECORD+7         ADVANCE PAST *PHASE                          
         CLI   0(R5),C' '                                                       
         BNE   *+12                FOUND FIRST NONBLANK                         
         LA    R5,1(R5)            ADVANCE POINTER                              
         B     *-12                LOOP                                         
*                                                                               
         TRT   0(10,R5),TRTBLANK   FIND BLANK OR COMMA                          
         SR    R1,R5               LENGTH OF TEST PHASE NAME                    
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSTNAME(0),0(R5)    SAVE THE TEST PHASE NAME                     
*                                                                               
         BCTR  R1,0                CUT OFF TEST LEVEL LETTER                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTNAME(0),0(R5)                                                 
*                                                                               
GPALNKX  DS    0H                                                               
         B     GPLMOD                                                           
         SPACE 3                                                                
GPGEN    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPGEN10             YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPGEN10  DS    0H                                                               
         CLI   RECORD,C'S'         SCREEN CARD?                                 
         BNE   GPGEN               NO: KEEP READING                             
*                                                                               
         MVC   OUTNAME(4),RECORD+4 ROOT NAME PPPP                               
         MVC   OUTNAME+4(2),RECORD+1 SUFFIX SS                                  
         MVC   TSTNAME,OUTNAME                                                  
         MVI   TSTNAME+6,C'A'      DEFAULT TEST SUFFIX                          
         CLI   RECORD+3,C' '                                                    
         BE    *+10                                                             
         MVC   TSTNAME+6(1),RECORD+3 TEST SUFFIX                                
         B     GPLMOD                                                           
         SPACE 3                                                                
GPAPG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPAPG10             YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPAPG10  DS    0H                                                               
         CLC   =C'PHASE    ',RECORD  IS THIS THE APG PHASE CARD?                
         BNE   GPAPG               NO: KEEP READING                             
*                                                                               
         MVC   OUTNAME(2),=CL2'AC'                                              
         MVC   OUTNAME+2(5),RECORD+9   CONSTRUCT PHASE NAME                     
         MVC   TSTNAME,OUTNAME                                                  
         MVC   TSTNAME+7(1),RECORD+9+5 TEST SUFFIX                              
         B     GPLMOD                                                           
         SPACE 3                                                                
GPDPG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPDPG10             YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPDPG10  DS    0H                                                               
         CLC   =C'         PHASE ',RECORD   IS THIS THE DPG PHASE CARD?         
         BNE   GPDPG               NO: KEEP READING                             
*                                                                               
         MVC   OUTNAME(6),RECORD+15  RETRIEVE PHASE NAME                        
         MVC   TSTNAME(7),RECORD+15  PHASE NAME + TEST SUFFIX                   
         B     GPLMOD                                                           
         SPACE 3                                                                
GPRRG    GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    GPRRG10             YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    *+6                 YES                                          
         DC    H'0'                FATAL ERROR READING PAN MEMBER               
         MVC   OUTNAME(9),=CL9'*UNKNOWN*'                                       
         B     GPX                                                              
*                                                                               
GPRRG10  DS    0H                                                               
         CLC   =C'PHASE    ',RECORD   IS THIS THE RRG PHASE CARD?               
         BNE   GPRRG               NO: KEEP READING                             
*                                                                               
         MVC   OUTNAME(4),=CL4'RERG'  CONSTRUCT PHASE NAME                      
         MVC   OUTNAME+4(2),RECORD+9                                            
         MVC   TSTNAME,OUTNAME                                                  
         MVC   TSTNAME+6(1),RECORD+9+2   TEST SUFFIX                            
         B     GPLMOD                                                           
         SPACE 3                                                                
GPSRCE   DS    0H                                                               
         CLI   PARMACT,VERIFY      ARE WE HERE VIA RVP ACTION?                  
         BNE   GPX                 NO: DON'T LOOK FOR "SRCE" FLOWER BOX         
*                                                                               
         LHI   R0,50               READ MAX OF 50 RECS BEFORE GIVING UP         
GPSRCE10 GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',MRFRMEM,RECORD           
         CLI   DMCB+8,0            SUCCESSFUL RECORD READ?                      
         BE    *+14                YES                                          
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    GPX                 YES                                          
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         CLC   =C'*$PANAPT01S$',RECORD  CHECK FOR EYE-CATCHER                   
         BE    *+12                "SRCE" LIBCODE FLOWER BOX PRESENT?           
         BCT   R0,GPSRCE10         NOT THIS RECORD: READ NEXT RECORD            
         B     GPX                 DON'T BOTHER READING ANY MORE RECS.          
*                                                                               
         MVC   RETCODE,=F'8'       YES: SET RC=8 (VERIFICATION FAILURE)         
         MVC   MESSAGE1,MSG10_1    SRCE LIBCODE COMMENT BLOCK PRESENT           
         MVC   MESSAGE1+67(L'MRFRMEM),MRFRMEM  MEMBER NAME                      
         MVC   MESSAGE2,MSG10_2                                                 
         MVI   ERRFLAG,C'Y'        FATAL ERROR                                  
         B     GPX                                                              
         EJECT                                                                  
GPLMOD   DS    0H                                                               
         OPEN  LMODIN              LOAD MODULE NAMES (ON INPUT)                 
GPLMOD5  DS    0H                                                               
         GET   LMODIN,LOADMODR                                                  
         CLC   LMODSRC,MRFRMEM     FIND MATCHING MEMBER RECORD                  
         BNE   GPLMOD5                                                          
*                                                                               
         MVC   LMODTST,TSTNAME                                                  
         MVC   LMODBASE,OUTNAME                                                 
         PUT   LOADMODS,LOADMODR                                                
*                                                                               
GPXCLOSE DS    0H                  EODAD FOR LMODIN (ONLY NEEDED FOR            
*                                   IDF DEBUGGING, WHEN LMODIN MIGHT            
*                                   BE DUMMY)                                   
         CLOSE LMODIN                                                           
*                                                                               
GPX      MVC   PPBPHASE,OUTNAME                                                 
         MVC   PPTPHASE,TSTNAME                                                 
         EDIT  (C3,OUTLEVEL),PPLEVEL   PAN LEVEL NUMBER                         
         MVC   PPUSER,OUTUSER      LAST UPDATE USERID                           
*                                                                               
         MVC   PUDATYR,OUTDATEU    YYYY                                         
         MVC   PUDATMON,OUTDATEU+4 MM                                           
         MVC   PUDATDAY,OUTDATEU+6 DD                                           
         MVC   PUTIME,OUTTIMEU     HH:MM:SS                                     
         MVC   PUTAMPM,SPACES      AM/PM                                        
*                                                                               
         CLC   PUTIME,SPACES       IF IT'S A VERY OLD PAN MEMBER, THE           
         BE    GPX2                 AUDIT INFORMATION MAY BE ABSENT.            
*                                   ASSUME THAT IF PUTIME CONTAINS              
*                                   DATA, THEN PUDATE DOES ALSO.                
         LA    R4,PUDATE                                                        
         LA    R5,PUTIME                                                        
         BRAS  RE,REALTIME         CONVERT SYS DATE&TIME TO REAL TIME           
         MVC   PUTAMPM,=C'PM'      ASSUME TIME PM                               
         PACK  HALF,PUTIME(2)                                                   
         CP    HALF,=P'11'         BUT IF HOUR IS <= 11, THEN...                
         BH    *+10                                                             
         MVC   PUTAMPM,=C'AM'      ...IT'S AM                                   
*                                                                               
         CP    HALF,=P'0'          FROM 00:00:00 THROUGH 00:59:59...            
         BNE   *+14                                                             
         MVC   PUTIME(2),=C'12'    HOUR IS 12(AM)                               
         B     GPX2                                                             
         CP    HALF,=P'13'         IS HOUR IS 1 THROUGH 12?                     
         BL    GPX2                YES: NO ADJUSTMENT NEEDED                    
         SP    HALF,=P'12'         HOURS 13..23: MUST SUBTRACT 12 HOURS         
         OI    HALF+1,X'0F'                                                     
         UNPK  PUTIME(2),HALF                                                   
*                                                                               
GPX2     DS    0H                                                               
         DROP  R6                                                               
         XIT1                                                                   
         SPACE 2                                                                
MSG10_1  DC    CL80'A SRCE Libcode comment block is present at the begi+        
               nning of member xxxxxxxxxx.'                                     
MSG10_2  DC    CL80'The comment is no longer valid. Edit the member, an+        
               d delete the comment.'                                           
         SPACE 2                                                                
TRTBLANK DC    256X'00'                                                         
         ORG   TRTBLANK+C' '                                                    
         DC    X'FF'                                                            
         ORG   TRTBLANK+C','                                                    
         DC    X'FF'                                                            
         ORG                                                                    
         SPACE 2                                                                
         DROP  RB                                                               
         LTORG                                                                  
GETPHASX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* SEND SMTP E-MAIL ON ERROR OR WARNING CONDITION.                               
***********************************************************************         
SNDMAIL  NTR1  BASE=(*,SNDMAILX),LABEL=*                                        
*                                                                               
         CLI   MAILSENT,C'Y'       HAVE WE SENT AN E-MAIL YET?                  
         BE    SNDM50              YES                                          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
         MVI   MAILSENT,C'Y'       REMEMBER THAT WE SENT AN E-MAIL              
*                                                                               
SNDM50   DS    0H                                                               
         MVC   SUBJMR#,PMR#        PUT MR# IN EMAIL SUBJECT                     
*                                                                               
         MVI   WORK,C' '              BUILD RECIPIENT LIST IN 'WORK'            
         MVC   WORK+1(L'WORK-1),WORK  INIT OUTPUT RECORD TO SPACES              
         LA    RF,WORK                                                          
         LA    RE,SUBMITTR                                                      
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   *-18                                                             
*&&UK                                                                           
         MVC   0(4,RF),=C'DDLO'                                                 
         LA    RF,4(RF)                                                         
*&&                                                                             
         CLI   CC_ADMIN,C'Y'       SEND E-MAIL COPY TO ADMINISTRATOR?           
         BNE   *+18                NO                                           
         MVI   0(RF),C','          DELIMITER BETWEEN RECIPIENTS                 
         MVC   1(L'ADMIN_EMAIL,RF),ADMIN_EMAIL  ALSO SEND TO ADMIN.             
         LA    RF,L'ADMIN_EMAIL+1(RF)                                           
*                                                                               
         CLC   PARMENV,=C'SBX'     NEVER CC MANAGER RE: SANDBOX                 
         BE    SNDM100                                                          
         CLI   CC_MANAG,C'Y'       SEND E-MAIL COPY TO MANAGER?                 
         BNE   SNDM100             NO                                           
         MVI   0(RF),C','          DELIMITER BETWEEN RECIPIENTS                 
         MVC   1(L'MANAGER_EMAIL,RF),MANAGER_EMAIL  ALSO SEND TO MANAG.         
*                                                                               
SNDM100  DS    0H                                                               
         CLI   WARNFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SUBJECT+11(8),=C'WARNING!'                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',WORK),('SUBJCTLQ',SUBJECT)             
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MESSAGE1)  MESSAGE BODY LINE 1         
         CLC   MESSAGE2,SPACES     IS THERE A MESSAGE BODY LINE 2 ?             
         BE    SNDM130             NO                                           
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',MESSAGE2)  MESSAGE BODY LINE 2         
*                                                                               
SNDM130  EQU   *                                                                
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*&&US                                                                           
ADMIN_EMAIL DC C'DEISDDNY'                                                      
*&&                                                                             
*&&UK                                                                           
ADMIN_EMAIL DC C'TCLEDDLO'                                                      
*&&                                                                             
*&&US                                                                           
MANAGER_EMAIL DC C'EJORDDNY'                                                    
*&&                                                                             
*&&UK                                                                           
MANAGER_EMAIL DC C'NSHEDDLO'                                                    
*&&                                                                             
SUBJECT  DC    C'PANAPT RVP ERROR!   MR# '                                      
SUBJMR#  DS    CL6                                                              
SUBJCTLQ EQU   *-SUBJECT                                                        
         SPACE 2                                                                
         LTORG                                                                  
SNDMAILX EQU   *                                                                
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* CONSTRUCT LIST OF NOTIFICATION RECIPIENTS FROM INVENTORY RECORDS.             
***********************************************************************         
NOTIFIES NTR1  BASE=(*,NOTIFIESX),LABEL=*                                       
*                                                                               
         XC    DMCB(24),DMCB       SO TABLE COUNTER IS CLEARED                  
NOTIFY10 DS    0H                                                               
*                                                                               
* THE DDSVRFY MODEL HAS POPULATED THE NTFYGRPS DATASET WITH ONE RECORD          
* FOR EACH MEMBER THAT HAS ANY NOTIFICATION GROUPS. THE FIRST 50 BYTES          
* OF EACH RECORD CONTAIN THE GROUP NAME(S). THOSE NAMES ARE EITHER              
* HARD-CODED IN THE DDSVRFY MODEL ITSELF, OR COME FROM THE INVENTORY            
* RECORD IN USER DATA FIELD 11.                                                 
*                                                                               
         GET   NTFYGRPS,RECORD                                                  
*                                                                               
         LA    R4,RECORD           PREPARE TO PARSE THIS FIELD                  
NOTIFY20 DS    0H                                                               
         MVI   WORK,C' '           PRESET VALUE TO SPACES                       
         MVC   WORK+1(L'WORK-1),WORK                                            
         LA    R1,WORK                                                          
NOTIFY30 CLI   0(R4),C' '          GROUP WILL END WITH COMMA OR BLANK           
         BE    NOTIFY40                                                         
         CLI   0(R4),C','                                                       
         BE    NOTIFY40                                                         
         MVC   0(1,R1),0(R4)       BUILD GROUP ONE CHARACTER AT A TIME          
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         B     NOTIFY30                                                         
*                                                                               
NOTIFY40 DS    0H                                                               
* DMCB+8 ALWAYS CONTAINS THE NUMBER OF RECORDS IN THE TABLE!!!                  
         GOTO1 =V(BINSRCH),DMCB,(X'01',WORK),A(GRPTABLE),,NTFYGRPLQ,   +        
               NTFYGRPLQ,MAXGRPS                                                
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                BINSRCH TABLE FULL: INCREASE MAXGRPS         
         CLI   0(R4),C' '          WAS THIS THE LAST GROUP IN THE LIST?         
         BE    NOTIFY10            YES: GET GROUPS FOR NEXT MEMBER              
         LA    R4,1(R4)            NO: BUMP PAST COMMA, GET NEXT GROUP          
         B     NOTIFY20                                                         
*                                                                               
NOTIFY50 DS    0H                                                               
         ICM   R0,15,DMCB+8        ANY GROUPS FOUND?                            
         BZ    NOTIFYX             NO                                           
*                                                                               
         LA    R4,PNOTIFYR         R4 = A(BEGINNING OF OUTPUT AREA)             
         LR    R7,R4               R7 = OUTPUT AREA ROW POINTER                 
         LARL  R5,GRPTABLE         START AT BEGINNING OF GROUP TABLE            
*                                                                               
NOTIFY60 DS    0H                                                               
         LARL  RF,NOTIFICATION_GROUPS  TABLE OF GROUPS AND ADDRESSES            
NOTIFY70 DS    0H                                                               
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID GROUP IN INVENTORY RECORD?!?         
         CLC   0(NTFYGRPLQ,RF),0(R5)  LOOK FOR MATCH ON GROUP NAME              
         BE    NOTIFY80            GOT IT                                       
         CLI   0(RF),X'FE'         SCAN FORWARD TO NEXT GROUP                   
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LA    RF,1(RF)            BUMP PAST X'FE'                              
         B     NOTIFY70                                                         
*                                                                               
NOTIFY80 DS    0H                                                               
         LA    RF,NTFYGRPLQ(RF)    BUMP PAST GROUP NAME                         
NOTIFY90 DS    0H                                                               
         CLI   0(RF),X'FE'         END OF LAST ADDRESS FOR THIS GROUP?          
         BE    NOTIFY95            YES                                          
         MVC   0(1,R7),0(RF)       BUILD ADDRESS ONE CHAR. AT A ATIME           
         LA    R7,1(R7)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),C','          END OF THE ADDRESS?                          
         BNE   NOTIFY90            NOT YET                                      
         MVI   0(R7),C','          DELIMIT THE ADDRESS                          
         LA    RF,1(RF)            BUMP PAST COMMA                              
         LA    R4,L'PNOTIFYR(R4)                                                
         LH    RE,NOTIFY#                                                       
         AHI   RE,1                INCREMENT TOTAL NUMBER TO NOTIFY             
         STH   RE,NOTIFY#                                                       
         LR    R7,R4               R7 = A(NEXT OUTPUT ADDRESS SLOT)             
         B     NOTIFY90                                                         
*                                                                               
NOTIFY95 DS    0H                                                               
         LA    R5,NTFYGRPLQ(R5)    PROCESS NEXT GROUP IN TABLE                  
         BCT   R0,NOTIFY60                                                      
*                                                                               
NOTIFYX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
NOTIFIESX EQU   *                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* PROCESS OUTPUT FROM PRIOR JIRA REST (CURL) CALL, TO MAKE SURE THAT            
* A VALID TICKET NUMBER IS ON THE MR.                                           
***********************************************************************         
VALTKT#  NTR1  BASE=(*,VALTKTXX),LABEL=*                                        
*                                                                               
* THE DDSVRFY MODEL HAS POPULATED THE CURLOUT DATASET WITH THE STDOUT           
* OUTPUT FROM THE CURL COMMAND. WE PARSE THAT OUTPUT HERE, LOOKING FOR          
* THE RESPONSE CODE FROM THE REST CALL TO JIRA.                                 
*                                                                               
VALTKT_GET DS    0H                                                             
         CLI   PTICKET,C'A'        TICKET NUMBER MUST START WITH ALPHA          
         BL    VALTKT_INVALID                                                   
         CLI   PTICKET,C'Z'                                                     
         BH    VALTKT_INVALID                                                   
*                                                                               
         GET   CURLOUT,RECORD                                                   
         CLC   =C'Response = ',RECORD                                           
         BNE   VALTKT_GET          READ UNTIL RESPONSE RECORD FOUND             
*                                                                               
         CLC   =C'200 ',RECORD+11  JIRA TICKET NUMBER FOUND?                    
         BNE   *+10                                                             
         SR    RF,RF               YES: HOPEFULLY IT'S THE RIGHT ONE            
         B     VALTKT_XIT          WE'RE GOOD TO GO                             
*                                                                               
         CLC   PTICKET,=CL16'EMERGENCY' TICKET # NOT PROVIDED?                  
         BNE   VALTKT_CHK_404                                                   
         MVC   MESSAGE1,MSG15_1    *** NO JIRA TICKET ***                       
         MVC   MESSAGE2,MSG15_2                                                 
         MVI   WARNFLAG,C'Y'       IT'S A WARNING                               
         MVI   CC_ADMIN,C'Y'       CC THE PANAPT ADMINISTRATOR                  
         MVI   CC_MANAG,C'Y'       CC MANAGEMENT                                
         LHI   RF,4                VERIFY THE MR, BUT WARN VIA E-MAIL           
         B     VALTKT_XIT                                                       
*                                                                               
VALTKT_CHK_404 DS 0H                                                            
         CLC   =C'404 ',RECORD+11  TICKET NUMBER NOT FOUND?                     
         BNE   VALTKT_CHK_550      NO: IT'S SOMETHING ELSE                      
*                                                                               
VALTKT_INVALID DS 0H                                                            
         MVC   MESSAGE1,MSG11_1    INVALID JIRA TICKET NUMBER                   
         MVC   MESSAGE1+34(16),PTICKET                                          
         MVC   MESSAGE2,MSG11_2                                                 
         LHI   RF,8                DO NOT VERIFY THIS MR: SET RC=8              
         B     VALTKT_XIT                                                       
*                                                                               
VALTKT_CHK_550 DS 0H                                                            
         CLC   =C'550 ',RECORD+11  SERVER UNAVAILABLE?                          
         BNE   VALTKT_UNK          NO: IT'S SOMETHING ELSE                      
         MVC   MESSAGE1,MSG12_1    JIRA SERVER UNAVAILABLE                      
         MVC   MESSAGE2,SPACES                                                  
         MVI   WARNFLAG,C'Y'       IT'S A WARNING                               
         MVI   CC_ADMIN,C'Y'       CC THE PANAPT ADMINISTRATOR                  
         LHI   RF,4                VERIFY THE MR, BUT WARN VIA E-MAIL           
         B     VALTKT_XIT                                                       
*                                                                               
VALTKT_UNK DS 0H                                                                
         MVC   MSG13_1N,RECORD+11  RESPONSE CODE                                
         CLC   MSG13_1N,SPACES                                                  
         BNE   *+10                                                             
         MVC   MSG13_1N,=CL20'<BLANKS>'                                         
         MVC   MESSAGE1,MSG13_1    UNEXPECTED RESPONSE CODE FROM JIRA           
         MVC   MESSAGE2,SPACES                                                  
         MVI   WARNFLAG,C'Y'       IT'S A WARNING                               
         MVI   CC_ADMIN,C'Y'       CC THE PANAPT ADMINISTRATOR                  
         LHI   RF,4                VERIFY THE MR, BUT WARN VIA E-MAIL           
         B     VALTKT_XIT                                                       
*                                                                               
VALTKT_NO_RESPONSE DS 0H                                                        
         MVC   MESSAGE1,MSG14_1    NO RESPONSE TO REST CALL                     
         MVC   MESSAGE2,MSG14_2                                                 
         MVI   WARNFLAG,C'Y'       IT'S A WARNING                               
         MVI   CC_ADMIN,C'Y'       CC THE PANAPT ADMINISTRATOR                  
         LHI   RF,4                VERIFY THE MR, BUT WARN VIA E-MAIL           
*                                                                               
VALTKT_XIT DS 0H                                                                
         XIT1  REGS=(RF)                                                        
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MSG11_1  DC    CL80'Verification Failure! JIRA ticket XXXXXXXXXXXXXXXX +        
               does not exist. If this is an'                                   
MSG11_2  DC    CL80'emergency MR promotion, enter "EMERGENCY" in the JI+        
               RA ticket number field.'                                         
MSG12_1  DC    CL80'Internal warning: JIRA server unavailable.'                 
MSG13_1  DC    CL80' '                                                          
         ORG   MSG13_1                                                          
         DC    C'Internal warning: unexpected Response code from JIRA: +        
               '                                                                
MSG13_1N DS    CL20                EXPECTING CL3, BUT WE'VE SEEN LONGER         
         ORG                                                                    
MSG14_1  DC    CL80'Internal warning: no Response record received in re+        
               ply to the REST call to the'                                     
MSG14_2  DC    CL80'JIRA database.'                                             
MSG15_1  DC    CL80'** No JIRA ticket number provided. This is permitte+        
               d only in an emergency.'                                         
MSG15_2  DC    CL80'Subsequent remediation is required.'                        
         SPACE 2                                                                
VALTKTXX EQU   *                                                                
         EJECT                                                                  
COMMWORK DS    0D                  COMMON STORAGE AREA                          
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
WORK     DS    CL133                                                            
WORK2    DS    CL132                                                            
RETCODE  DC    F'0'                RETURN CODE                                  
AFRSTLVL DS    A                   A(FIRST LEVEL) IN LIBCODE EXTRACT            
ALASTLVL DS    A                   A(LAST LEVEL) IN LIBCODE EXTRACT             
NOTIFY#  DC    H'0'                # OF NOTIFIC. RECIPIENT ADDRESSES            
ERRFLAG  DS    C                   ERROR FOUND: Y/N                             
MAILSENT DC    C'N'                'Y' = AN EMAIL WAS SENT                      
LEVELPOS DS    XL2                 CURRENT LEVEL RELATIVE POSITION              
ACCSMETH DS    CL2                 LIBCODE ACCESS METHOD                        
LIBCDGRP DC    C'        '         TO ENSURE LIBCODE CONSISTENCY IN MR          
PRKSFLAG DC    C' '                TO ENSURE /K LIBCODE CONSISTENCY             
CC_ADMIN DC    C'N'                SEND E-MAIL CC TO ADMINISTRATOR              
CC_MANAG DC    C'N'                SEND E-MAIL CC TO MANAGER                    
WARNFLAG DC    C'N'                SEND WARNING (AS OPPOSED TO ERROR)           
MESSAGE1 DS    CL80                E-MAILS: MESSAGE BODY LINE 1                 
MESSAGE2 DS    CL80                E-MAILS: MESSAGE BODY LINE 2                 
*                                                                               
ORIGDSN  DS    CL44                ORIGIN PANLIB                                
PRODDSN  DS    CL44                PRODUCTION PANLIB                            
RELATLBC DS    CL7                 RELATED LIBCODE                              
RELATDSN DS    CL44                RELATED LIBCODE PANLIB                       
*                                                                               
MRFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
LOCKCRDS DCB   DDNAME=LOCKCRDS,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB              
UNLKCRDS DCB   DDNAME=UNLKCRDS,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB              
MEMTAB   DCB   DDNAME=MEMTAB,DSORG=PS,MACRF=PM  ** LRECL=133,RECFM=FB           
MEMTABJS DCB   DDNAME=MEMTABJS,DSORG=PS,MACRF=PM  ** LRECL=132,RECFM=FB         
LIBCODES DCB   DDNAME=LIBCODES,DSORG=PS,MACRF=GM,LRECL=20004,RECFM=VBS          
LEVELS   DCB   DDNAME=LEVELS,DSORG=PS,MACRF=GM,LRECL=133,RECFM=FB               
LMODIN   DCB   DDNAME=LMODIN,DSORG=PS,LRECL=80,RECFM=FB,MACRF=GM,      +        
               EODAD=GPXCLOSE                                                   
CURLOUT  DCB   DDNAME=CURLOUT,DSORG=PS,LRECL=80,RECFM=FB,MACRF=GM,     +        
               EODAD=VALTKT_NO_RESPONSE                                         
CRDS5955 DCB   DDNAME=CRDS5955,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
*&&DO                                                                           
NTFYGRPS DCB   DDNAME=NTFYGRPS,DSORG=PS,LRECL=80,RECFM=FB,MACRF=GM,    +        
               EODAD=NOTIFY50                                                   
*&&                                                                             
LOADMODS DCB   DDNAME=LOADMODS,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
*                                                                               
RECORD   DC    CL80' '                                                          
         EJECT                                                                  
OUTNAME  DS    CL10                                                             
OUTLEVEL DS    CL3                 LEVEL NUMBER                                 
OUTUSER  DS    CL8                 USERID OF LAST UPDATE                        
OUTDATEU DS    CL8                 DATE OF LAST UPDATE (YYYYMMDD)               
OUTTIMEU DS    CL8                 TIME OF LAST UPDATE (HH:MM:SS)               
TSTNAME  DS    CL10                                                             
         SPACE 3                                                                
* THIS STRUCTURE IS READ BY THE JRXUTIL CLIST AND BY DDPAPTCLAS.                
*                                                                               
LOADMODR DS    0CL80               LMODIN/LOADMODS RECORD                       
LMODSRC  DS    CL10                SOURCE MEMBER NAME                           
         DS    CL2                                                              
LMODTST  DS    CL8                 TEST PHASE NAME                              
         DS    CL2                                                              
LMODBASE DS    CL8                 BASE PHASE NAME                              
         DS    CL2                                                              
         DS    CL8                 BACKUP PHASE NAME                            
         DS    CL2                                                              
         DS    CL7                 LIBCODE                                      
         DS    C                                                                
         DS    CL6                 (PROBABLY UNUSED: SEE JRXUTIL)               
         DS    CL3                                                              
         DS    CL4                 'YES' IF LIBCODE USES STGE, OW 'NO'          
         DS    C                                                                
         DS    CL8                 TEMPORARY PHASE NAME ("@" SUFFIX)            
         ORG                                                                    
         DS    CL(80-(*-LOADMODR))  SPARE                                       
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**LEVEL TABLE***'                                              
LEVELTAB DC    (MAX_PANAPT_LEVELS)XL(LEVELTBQ)'00'                              
         DC    X'00'               EOT                                          
MAX_PANAPT_LEVELS EQU 16                                                        
         SPACE 3                                                                
LEVELTBD DSECT                                                                  
LVLSHORT DS    CL4                 LEVEL SHORT NAME                             
LVLABBRV DS    CL2                 LEVEL ABBREVIATION                           
LVLSPS   DS    XL2                 LEVEL RELATIVE SYSTEM POSITION               
LVLREQD  DS    C                   'Y' = LEVEL IS REQUIRED                      
LVLAVREQ DS    C                   'Y' = LEVEL REQUIRES VERIFICATION(S)         
*                                    AND/OR APPROVAL(S) FOR PROMOTION           
LVLDATEM DS    CL8                 DATE MOVED TO THIS LEVEL                     
*                                    ALWAYS NULLS FOR TEST LEVEL                
*                                    BLANKS IF NOT MOVED TO THIS LEVEL          
LEVELTBQ EQU   *-LEVELTBD                                                       
         SPACE 3                                                                
APCS5103_VIA_DFSORT_DSECT DSECT                                                 
APCS5103_LVLSHORT DS    CL4        LEVEL SHORT NAME                             
APCS5103_LVLABBRV DS    CL2        LEVEL ABBREVIATION                           
         EJECT                                                                  
PAPTMRM  CSECT                                                                  
         DS    0D                                                               
PMR#     DC    CL6' '              MOVE REQUEST NUMBER                          
*                                                                               
PADATE   DS    0CL10               ACTION DATE                                  
*&&US                                                                           
PADATMON DC    CL2' ',C'/'         MONTH                                        
PADATDAY DC    CL2' ',C'/'         DAY                                          
*&&                                                                             
*&&UK                                                                           
PADATDAY DC    CL2' ',C'/'         DAY                                          
PADATMON DC    CL2' ',C'/'         MONTH                                        
*&&                                                                             
PADATYR  DC    CL4' '              YEAR                                         
*                                                                               
PATIME   DS    0CL8                ACTION TIME                                  
PATIMHR  DC    CL2' ',C':'         HOUR                                         
PATIMMIN DC    CL2' ',C':'         MINUTE                                       
PATIMSEC DC    CL2' '              SECOND                                       
*                                                                               
PCDATE   DS    0CL10               CREATION DATE                                
*&&US                                                                           
PCDATMON DC    CL2' ',C'/'         MONTH                                        
PCDATDAY DC    CL2' ',C'/'         DAY                                          
*&&                                                                             
*&&UK                                                                           
PCDATDAY DC    CL2' ',C'/'         DAY                                          
PCDATMON DC    CL2' ',C'/'         MONTH                                        
*&&                                                                             
PCDATYR  DC    CL4' '              YEAR                                         
*                                                                               
PCTIME   DS    0CL8                CREATION TIME                                
PCTIMHR  DC    CL2' ',C':'         HOUR                                         
PCTIMMIN DC    CL2' ',C':'         MINUTE                                       
PCTIMSEC DC    CL2' '              SECOND                                       
*                                                                               
PPDATE   DS    0CL10               APPROVAL DATE                                
*&&US                                                                           
PPDATMON DC    CL2' ',C'/'         MONTH                                        
PPDATDAY DC    CL2' ',C'/'         DAY                                          
*&&                                                                             
*&&UK                                                                           
PPDATDAY DC    CL2' ',C'/'         DAY                                          
PPDATMON DC    CL2' ',C'/'         MONTH                                        
*&&                                                                             
PPDATYR  DC    CL4' '              YEAR                                         
*                                                                               
PPTIME   DS    0CL8                APPROVAL TIME                                
PPTIMHR  DC    CL2' ',C':'         HOUR                                         
PPTIMMIN DC    CL2' ',C':'         MINUTE                                       
PPTIMSEC DC    CL2' '              SECOND                                       
*                                                                               
PACTION  DC    CL12' '             VERIFICATION/PROMOTION/BACKOUT               
*                                                                               
PAPPRVR  DC    CL8' '              APPROVER                                     
PAPTLEVL DC    CL4' '              LEVEL                                        
POWNER   DC    CL8' '              OWNER OF THIS MR                             
SUBMITTR DC    CL8' ',C' '         LAST UPDATED BY                              
PSTA     DC    CL6' '              STATUS OF MOVE REQUEST OR ACTION             
*                                                                               
PTICKET  DC    CL16' '             TICKET                                       
PDES     DC    CL55' '             DESCRIPTION                                  
PENVIRON DC    CL3' '              ENVIRONMENT                                  
*                                    SBX = SANDBOX                              
*                                    DDS = DDSNY OR DDSUK                       
PMOVETYP DC    C' '                MOVE TYPE                                    
PORIGMR# DC    C'      '           ORIGINAL MR # (IF THIS IS A REWORK)          
PEARSTOP DC    C'    '             EARLY STOP LEVEL                             
*&&US                                                                           
PCTRYORG DC    C'US'               MR ORIGINATED IN THE US                      
*&&                                                                             
*&&UK                                                                           
PCTRYORG DC    C'UK'               MR ORIGINATED IN THE UK                      
*&&                                                                             
*                                                                               
PUPDMTAB DC    C'N'                UPDATE MEMBER TABLE FLAG (Y/N)               
*                                                                               
PPBPHASE DC    CL10' '             PHASE NAME                                   
PPTPHASE DC    CL10' '             TEST PHASE NAME                              
PPLEVEL  DC    CL3' '              LEVEL NUMBER                                 
PPUSER   DC    CL8' '              USERID OF LAST UPDATE                        
*                                                                               
MEMRECMR DS    CL6                 LAST MR# TO PROMOTE MEMBER TO PROD           
MEMITMF# DS    CL16                LAST ITMF TO PROMOTE MEMBER TO PROD          
*                                                                               
PUDATE   DC    0CL10' '            DATE OF LAST UPDATE                          
*&&US                                                                           
PUDATMON DC    CL2' ',C'/'         MONTH                                        
PUDATDAY DC    CL2' ',C'/'         DAY                                          
*&&                                                                             
*&&UK                                                                           
PUDATDAY DC    CL2' ',C'/'         DAY                                          
PUDATMON DC    CL2' ',C'/'         MONTH                                        
*&&                                                                             
PUDATYR  DC    CL4' '              YEAR                                         
*                                                                               
PUTIMEFM DC    0CL11               FORMATTED TIME                               
PUTIME   DC    CL8' '              TIME OF LAST UPDATE (HH:MM:SS)               
         DC    C' '                                                             
PUTAMPM  DC    CL2' '              "AM" OR "PM"                                 
*                                                                               
PARMACT  DC    C' '                MR ACTION                                    
VERIFY   EQU   C'V'                 WE GOT HERE VIA RVP                         
PROMOTE  EQU   C'M'                 WE GOT HERE VIA 'SUB' TO PROMOTE            
BACKOUT  EQU   C'B'                 WE GOT HERE VIA 'SUB' TO BACKOUT            
PARMLVL  DC    CL4' '              MR LEVEL                                     
PARMENV  DC    CL3' '              ENVIRONMENT PREFIX (SBX OR DDS)              
PARMUSER DC    CL8' '              SUBMITTER'S TSO USERID                       
*                                                                               
         DS    0H                  NOTIFIC. RECIPIENT E-MAIL ADDRESSES          
PNOTIFYR DC    (MAXNTFYQ)CL(60+1)' '                                            
MAXNTFYQ EQU   20                  MAXIMUM NO. OF RECIPIENT ADDRESSES           
*                                                                               
* MAXMEMQ IS ARBITRARY. IT IS THE MAXIMUM NUMBER OF MEMBERS SUPPORTED           
* WITHIN THIS PROGRAM, AND IT CAN BE INCREASED AT ANY TIME. IT                  
* IS *NOT* A PANAPT LIMIT ON THE NUMBER OF MEMBERS PER MOVE REQUEST.            
*                                                                               
MAXMEMQ  EQU   250                 MAX. NO. OF MEMBER TABLE ENTRIES             
*                                                                               
         DS    0H                  MEMBER NAME                                  
PMEMBER  DC    (MAXMEMQ)CL(L'MRFRMEM+1)' '                                      
         DS    0H                  LIBCODE (FORMAT: C'LIBC/SUB')                
PLIBCODE DC    (MAXMEMQ)CL(L'MRLIBC+1+1)' '                                     
         DS    0H                  "TO" USER DATA                               
PTOUSRDT DC    (MAXMEMQ)CL(L'MRTODATA+1)' '                                     
         DS    0H                  OBJECT (LIVE OUTPUT OBJECT NAME)             
POBJECT  DC    (MAXMEMQ)CL(L'MRFRMEM+1)' '                                      
POBJECTLQ EQU   *-POBJECT                                                       
         DS    0H                  OBJECT (TEST PHASE NAME)                     
PTOBJECT DC    (MAXMEMQ)CL(L'MRFRMEM+1)' '                                      
         DS    0H                  PAN LEVEL NUM.                               
PPLVL    DC    (MAXMEMQ)CL(L'PPLEVEL+1)' '                                      
         DS    0H                  USERID OF LAST UPDATE                        
PPUSR    DC    (MAXMEMQ)CL(L'PPUSER+1)' '                                       
         DS    0H                  DATE/TIME OF LAST UPDATE                     
PPDTTMU  DC    (MAXMEMQ)CL(L'PUDATE+1+L'PUTIMEFM+1)' '                          
         DS    0H                  LAST MR# TO PROMOTE TO PROD                  
PREVMR#  DC    (MAXMEMQ)CL(L'MEMRECMR+1)' '                                     
         DS    0H                  LAST JIRA ITMF# TO PROMOTE TO PROD           
PREVITMF DC    (MAXMEMQ)CL(L'MEMITMF#+1)' '                                     
*                                                                               
         DS    0H                                                               
BLOCK    DS    (POBJECTLQ)C                                                     
*                                                                               
         DS    0H                                                               
GRPTABLE DC    (MAXGRPS)CL(NTFYGRPLQ)' '  TABLE OF NOTIFICATION GROUPS          
MAXGRPS  EQU   20                                                               
         EJECT                                                                  
       ++INCLUDE DDPAPTNTFY                                                     
         EJECT                                                                  
         DS    0H                                                               
AUDITINF DS    676C                PAN AUDIT DATA                               
*                                                                               
         DS    0H                                                               
REC      DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*LIBCODE'                                                      
LIBCRECR DS    F                   RDW                                          
LIBCREC  DS    20000X              LIBCODE EXTRACT RECORD                       
         EJECT                                                                  
         DS    0L                                                               
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
         SPACE 3                                                                
         EJECT                                                                  
         PRINT GEN                                                              
APAMMDES_DSECT APAMMDES                                                         
         EJECT                                                                  
APAMMVER APAMMVER                                                               
         EJECT                                                                  
APAMMAPP APAMMAPP                                                               
         EJECT                                                                  
         COPY APAMLIB2                                                          
         EJECT                                                                  
MMMBRD   DSECT                                                                  
         APAMMMBR                                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
       ++INCLUDE DDPAN0UPD                                                      
         EJECT                                                                  
AUDITD   DSECT                                                                  
       ++INCLUDE DDPANAUDIT                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091DDPAPTMRM 07/24/20'                                      
         END                                                                    
