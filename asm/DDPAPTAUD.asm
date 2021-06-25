*          DATA SET DDPAPTAUD  AT LEVEL 004 AS OF 10/07/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTAUDA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
         TITLE 'PANAPT: EXTRACT FOR AUDIT REPORT'                               
*                                                                               
* THIS PROGRAM READS THE OUTPUT OF THE PANAPT APCS5955 UTILITY, WHICH           
* IS A FLAT FILE CONTAINING A DUMP OF THE PANAPT MOVE REQUEST DATABASE.         
* THE OUTPUT OF THIS PROGRAM IS A TAB-DELIMITED FILE, SUITABLE FOR              
* IMPORTING INTO EXCEL. IT CAN THEN BE EDITED AND PROVIDED TO AUDITORS.         
*                                                                               
* STARTDATE= AND ENDDATE= ARE TWO REQUIRED SYSIN PARAMETER CARDS,               
* INDICATING THE REQUEST PERIOD. ONLY MOVE REQUESTS WHICH WERE "MOVED           
* TO PRODUCTION" DURING THIS WINDOW OF TIME WILL BE INCLUDED IN THE             
* OUTPUT FILE.                                                                  
*                                                                               
* THE IDEA IS TO PRODUCE A SPREADSHEET SHOWING ALL MOVE REQUESTS WHICH          
* HAVE BEEN "MOVED TO PRODUCTION" WITHIN A GIVEN TIMEFRAME. THE WINDOW          
* OF TIME IS PROVIDED VIA TWO REQUIRED PARAMETER CARDS:                         
*  STARTDATE=<DATVAL VALIDATED DATE>                                            
*  ENDDATE=<DATVAL VALIDATED DATE>                                              
*                                                                               
* NOTE: IF A MR HAS A JIRA TICKET NUMBER OF "EMERGENCY", THEN WE WANT           
*       TO SHOW THE TRUE JIRA TICKET NUMBER (AS FOUND IN THE JIRA               
*       DATABASE). SO THERE IS A HARD-CODED TABLE OF SUCH MOVE REQUESTS         
*       WHICH MUST BE UPDATED WHENEVER THERE IS AN "EMERGENCY" MOVE             
*       REQUEST. IF THIS TABLE ISN'T KEPT UP-TO-DATE, THEN WE HAVE TO           
*       ENTER THE TICKET NUMBER MANUALLY IN THE GENERATED EXCEL                 
*       SPREADSHEET.                                                            
*                                                                               
      EJECT                                                                     
      APAMMDES                                                                  
      EJECT                                                                     
*                                                                               
PAPTAUD  CSECT                                                                  
*                                                                               
      PRINT NOGEN                                                               
*                                                                               
      NBASE 0,PAPTAUD,=V(REGSAVE),RA                                            
*                                                                               
      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                        
      CLC   =C'STARTDATE=',CARD                                                 
      JNE   *+2                 REQUIRED PARAMETER CARD MISSING                 
      GOTO1 =V(DATVAL),DMCB,CARD+10,DUB                                         
      OC    DMCB(4),DMCB                                                        
      JZ    *+2                 INVALID START DATE                              
      GOTO1 VDATCON,DMCB,DUB,(23,STARTDAT)                                      
*                                                                               
      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                        
      CLC   =C'ENDDATE=',CARD                                                   
      JNE   *+2                 REQUIRED PARAMETER CARD MISSING                 
      GOTO1 =V(DATVAL),DMCB,CARD+8,DUB                                          
      OC    DMCB(4),DMCB                                                        
      JZ    *+2                 INVALID END DATE                                
      GOTO1 VDATCON,DMCB,DUB,(23,ENDDAT)                                        
*                                                                               
      OPEN  MRFILE              OUTPUT OF APCS5955                              
      OPEN  (OUTFULL,OUTPUT)    TAB-DELIMITED FILE (ALL COLUMNS)                
      OPEN  (OUTAUDIT,OUTPUT)   TAB-DELIMITED FILE (COLUMNS FOR AUDIT)          
*                                                                               
      LA    R2,SEP_REC                                                          
      PUT   OUTFULL,(R2)        SEP=<TAB> FOR EXCEL                             
      PUT   OUTAUDIT,(R2)                                                       
      LA    R2,HEADINGS                                                         
      PUT   OUTFULL,(R2)        COLUMN HEADINGS (ALL)                           
      MVC   PAPTREC(HEADINGS_FOR_AUDIT_REPORT),0(R2)                            
      PUT   OUTAUDIT,PAPTREC    COLUMN HEADINGS (FOR AUDIT REPORT)              
*                                                                               
      LA    R6,PAPTREC+4                                                        
      USING APAMMDES,R6                                                         
*                                                                               
      DO INF                    PROCESS UNTIL EOF                               
*                                                                               
        GET   MRFILE,PAPTREC                                                    
*                                                                               
        IF CLC,DESTYPE,NE,=C'01',ORIF,  ONLY LOOK AT MR DESCRIP. RECS.          
          CLC,=C'RW ',EQ,DESNAME,AND,   SKIP COPY FOR REWORK MOVE REQS.         
          CLI,DESNAME_DELIMITER,EQ,C':',ORIF,                                   
          CLC,DESSTAT,NE,=C'MVP   ',AND,  SKIP IF NOT AT PROD OR PRKS           
          CLC,DESSTAT,NE,=C'APP   ',AND,                                        
          CLC,DESSTAT,NE,=C'AWP   ',ORIF,                                       
          CLC,DESCMOVC,EQ,=X'0000'        SKIP IF NEVER PROMOTED AT ALL         
            ITERATE ,                                                           
        ENDIF ,                                                                 
*                                                                               
        MVC   O_DESNUMB,DESNUMB   MR #                                          
*                                                                               
        MVC   O_DESSTAT,DESSTAT   MR STATUS                                     
        MVC   O_DESCLSN,DESCLSN   MR CURRENT LEVEL (E.G., PROD, STGE)           
        MVC   O_DESMTYPE,DESMTYPE MR MOVE TYPE                                  
        EDIT  DES#MEM,O_DES#MEM   NUMBER OF MEMBER RECORDS                      
*                                                                               
        MVC   O_DESSRREQ,DESSRREQ JIRA TICKET #                                 
*                                                                               
        IF (CLC,O_DESSRREQ,EQ,=CL16'EMERGENCY')                                 
          MVI O_EMERGENCY_FLAG,C'Y'                                             
        ELSE ,                                                                  
          MVI O_EMERGENCY_FLAG,C' '                                             
        ENDIF ,                                                                 
*                                                                               
        LA    RF,EMERGENCY_MOVE_REQUESTS                                        
        USING EMERGENCYD,RF                                                     
        DO WHILE=(CLI,0(RF),NE,X'FF')   STOP AT EOT                             
          IF (CLC,DESNUMB,EQ,EMER_MR#)  WAS THIS AN EMERGENCY MR?               
            MVC   O_DESSRREQ,EMER_TKT   YES: USE HARD-CODED JIRA TKT#           
            DOEXIT ,                                                            
          ENDIF ,                                                               
        LA    RF,EMER_LQ(,RF)                                                   
        ENDDO ,                                                                 
        DROP  RF                                                                
*                                                                               
        MVC   O_JIRA_PROJECT,SPACES                                             
        IF (CLC,=C'TICKETNUMBERHERE',EQ,O_DESSRREQ)                             
          MVI   BYTE,X'FF'          NO JIRA PROJECT                             
        ELSE ,                                                                  
          MVI   BYTE,0                                                          
        ENDIF ,                                                                 
*                                                                               
        MVC   O_JIRA#,SPACES                                                    
        LA    RE,O_JIRA#          JIRA TICKET # (FOR "FRIENDLY NAME")           
        LA    RF,O_DESSRREQ                                                     
        LA    R2,O_JIRA_PROJECT                                                 
        LHI   R0,L'O_JIRA#                                                      
*                                                                               
        DO FROM=(R0)                                                            
          MVC 0(1,RE),0(RF)                                                     
          IF (CLI,BYTE,NE,X'FF'),AND,(C,R2,NE,=A(O_JIRA_PROJECT_X))             
            IF (CLI,0(RF),EQ,C'-')                                              
              MVI BYTE,X'FF'      WE GOT THE JIRA PROJECT                       
            ELSE ,                                                              
              MVC 0(1,R2),0(RF)                                                 
              LA  R2,1(,R2)                                                     
            ENDIF ,                                                             
          ENDIF ,                                                               
          LA RE,1(,RE)                                                          
          LA RF,1(,RF)                                                          
          IF (CLI,0(RF),EQ,C' ')                                                
            DOEXIT ,                                                            
          ENDIF ,                                                               
        ENDDO ,                                                                 
*                                                                               
        MVC   0(2,RE),=C'")'                                                    
        LA    RE,2(,RE)                                                         
        DO WHILE=(C,RE,NE,=A(O_JIRA#_X))                                        
          MVI   0(RE),C' '                                                      
          LA    RE,1(,RE)                                                       
        ENDDO ,                                                                 
*                                                                               
        MVC   O_DESNAME,DESNAME   MR "NAME"                                     
*                                                                               
*                                  "FINAL" AND "NEXT" MOVE DATES                
        GOTO1 VDATCON,DMCB,(9,DESFMVDA),(23,O_DESFMVDA)                         
        GOTO1 VDATCON,DMCB,(9,DESNXMDA),(23,O_DESNXMDA)                         
*                                                                               
        MVC   O_DESESTOP,DESESTOP EARLY STOP LEVEL                              
        MVC   O_DESADDID,DESADDID MR OWNER                                      
*                                                                               
*                                  MR ADD DATE AND TIME                         
        GOTO1 VDATCON,DMCB,(9,DESADDDA),(23,O_DESADDDA)                         
        MVC   O_DESADDHH,DESADDHH                                               
        MVC   O_DESADDMN,DESADDMN                                               
        MVC   O_DESADDSS,DESADDSS                                               
*                                                                               
        MVC   O_DESUPDID,DESUPDID LAST UPDATE USERID                            
*                                                                               
*                                  MR LAST UPDATE DATE AND TIME                 
        GOTO1 VDATCON,DMCB,(9,DESUPDDA),(23,O_DESUPDDA)                         
        MVC   O_DESUPDHH,DESUPDHH                                               
        MVC   O_DESUPDMI,DESUPDMI                                               
        MVC   O_DESUPDSS,DESUPDSS                                               
*                                                                               
        MVC   O_DESDESCR,DESDESCR MR DESCRIPTION                                
        LA    RF,O_DESDESCR                                                     
        LHI   R0,L'O_DESDESCR                                                   
        DO FROM=(R0)                                                            
          IF (CLI,0(RF),EQ,C'"')  CHANGE DOUBLE-QUOTES TO SINGLE-QUOTES         
            MVI   0(RF),C''''                                                   
          ENDIF ,                                                               
          LA    RF,1(,RF)                                                       
        ENDDO ,                                                                 
*                                                                               
        MVC   O_DATE_TO_PROD,NODATE                                             
*                                                                               
        MVC   O_DESMRCSN_STGE,SPACES                                            
        MVC   O_DESMRCDS_STGE,NODATE                                            
        MVC   O_DESMRCDM_STGE,NODATE                                            
        MVC   O_DESMRCTM_STGE,NOTIME                                            
*                                                                               
        MVC   O_DESMRCSN_PRKS,SPACES                                            
        MVC   O_DESMRCDS_PRKS,NODATE                                            
        MVC   O_DESMRCDM_PRKS,NODATE                                            
        MVC   O_DESMRCTM_PRKS,NOTIME                                            
*                                                                               
        MVC   O_DESMRCSN_PROD,SPACES                                            
        MVC   O_DESMRCDS_PROD,NODATE                                            
        MVC   O_DESMRCDM_PROD,NODATE                                            
        MVC   O_DESMRCTM_PROD,NOTIME                                            
*                                                                               
        SR    R7,R7                                                             
        ICM   R7,3,DESEXCNT       # EXPANDED DESCRIPTION RECORDS                
        MHI   R7,L'DESXPDES                                                     
        LA    R7,DESXPDES(R7)     R7 = A(DESMRMVC AREA)                         
M       USING DESMRCID,R7                                                       
        SR    R5,R5                                                             
        ICM   R5,3,DESCMOVC       COMPLETED MOVE LEVEL COUNT                    
*                                                                               
        DO FROM=(R5)              CHECK ALL PROMOTION LEVELS                    
          SELECT CLC,M.DESMRCSN,EQ                                              
            WHEN (=C'STGE')                                                     
              MVC O_DESMRCSN_STGE,M.DESMRCSN                                    
              IF (CLC,M.DESMRCDS,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDS),(23,O_DESMRCDS_STGE)          
              ENDIF ,                                                           
              IF (CLC,M.DESMRCDM,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDM),(23,O_DESMRCDM_STGE)          
              ENDIF ,                                                           
              MVC O_DESMRCHH_STGE,M.DESMRCHH                                    
              MVC O_DESMRCMM_STGE,M.DESMRCMM                                    
              MVC O_DESMRCSS_STGE,M.DESMRCSS                                    
*                                                                               
            WHEN (=C'PRKS')                                                     
              MVC O_DESMRCSN_PRKS,M.DESMRCSN                                    
              IF (CLC,M.DESMRCDS,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDS),(23,O_DESMRCDS_PRKS)          
              ENDIF ,                                                           
              IF (CLC,M.DESMRCDM,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDM),(23,O_DESMRCDM_PRKS)          
              ENDIF ,                                                           
              MVC O_DESMRCHH_PRKS,M.DESMRCHH                                    
              MVC O_DESMRCMM_PRKS,M.DESMRCMM                                    
              MVC O_DESMRCSS_PRKS,M.DESMRCSS                                    
*                                                                               
            WHEN (=C'PROD')                                                     
              MVC O_DESMRCSN_PROD,M.DESMRCSN                                    
              IF (CLC,M.DESMRCDS,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDS),(23,O_DESMRCDS_PROD)          
              ENDIF ,                                                           
              IF (CLC,M.DESMRCDM,NE,SPACES)                                     
                GOTO1 VDATCON,DMCB,(9,M.DESMRCDM),(23,O_DESMRCDM_PROD)          
              ENDIF ,                                                           
              MVC O_DESMRCHH_PROD,M.DESMRCHH                                    
              MVC O_DESMRCMM_PROD,M.DESMRCMM                                    
              MVC O_DESMRCSS_PROD,M.DESMRCSS                                    
*                                                                               
            OTHRWISE                                                            
              J *+2               UNDEFINED LEVEL ?!?                           
*                                                                               
          ENDSEL ,                                                              
*                                                                               
          LA    R7,L'DESMRMVC(R7) BUMP TO NEXT PROMOTION HISTORY ENTRY          
*                                                                               
        ENDDO ,                   CHECK NEXT PROMOTION LEVEL                    
        DROP  M                                                                 
*                                                                               
* DERIVE THE 'MOVED TO PROD' DATE, BASED ON THE MR STATUS AND ITS               
* PROMOTION HISTORY.                                                            
* IF STATUS IS AWP OR APP, THEN USE THE PRKS PROMOTION DATE (WHICH              
* MIGHT BE NULLS, INDICATING THAT THE MR IS NOT YET IN PRODUCTION).             
* IF STATUS IS MVP, AND IF THERE IS A PRKS PROMOTION DATE, USE THE PRKS         
* DATE. OTHERWISE, USE THE PROD PROMOTION DATE.                                 
*                                                                               
        IF (CLC,O_DESSTAT,EQ,=C'AWP   '),OR,                                    
           (CLC,O_DESSTAT,EQ,=C'APP   '),OR,                                    
           (CLC,O_DESSTAT,NE,=C'MVP   '),OR,                                    
           (CLC,O_DESMRCDM_PRKS,NE,NODATE)                                      
             MVC O_DATE_TO_PROD,O_DESMRCDM_PRKS  USE PRKS MOVE DATE             
        ELSE ,                                                                  
             MVC O_DATE_TO_PROD,O_DESMRCDM_PROD  USE PROD MOVE DATE             
        ENDIF ,                                                                 
*                                                                               
        IF (CLC,O_DATE_TO_PROD,EQ,NODATE)                                       
          ITERATE ,                 SKIP IF IT NEVER WENT TO PROD ENV.          
        ENDIF ,                                                                 
*                                                                               
        IF (CLC,O_DATE_TO_PROD,L,STARTDAT),OR,  WITHIN REQUEST RANGE?           
           (CLC,O_DATE_TO_PROD,H,ENDDAT)                                        
          ITERATE ,                               NO: SKIP IT                   
        ENDIF ,                                                                 
*                                                                               
        DROP  R6                                                                
*                                                                               
        PUT   OUTFULL,O_DES_RECORD    ALL COLUMNS                               
        PUT   OUTAUDIT,O_DES_RECORD   AUDIT REPORT COLUMNS                      
*                                                                               
      ENDDO ,                     READ NEXT RECORD                              
*                                                                               
CLOSE DS    0H                                                                  
      CLOSE MRFILE                                                              
      CLOSE OUTFULL                                                             
      CLOSE OUTAUDIT                                                            
*                                                                               
      XBASE                                                                     
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
BYTE     DS    X                                                                
WORK     DS    CL17                                                             
STARTDAT DS    CL10                YYYY-MM-DD                                   
ENDDAT   DS    CL10                YYYY-MM-DD                                   
VDATCON  DC    V(DATCON)                                                        
*                                                                               
LRECLQ   EQU   600                                                              
*                                                                               
MRFILE   DCB   DDNAME=MRFILE,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
OUTFULL  DCB   DDNAME=OUTFULL,DSORG=PS,MACRF=PM,LRECL=LRECLQ,RECFM=FB           
OUTAUDIT DCB   DDNAME=OUTAUDIT,DSORG=PS,MACRF=PM,RECFM=FB,             +        
               LRECL=COLUMNS_FOR_AUDIT_REPORT                                   
*                                                                               
SPACES   DC    CL80' '                                                          
*                                                                               
NODATE   DC    C'    -  -  '       YYYY-MM-DD                                   
NOTIME   DC    C'  :  :  '         HH:MM:SS                                     
*                                                                               
EMERGENCY_MOVE_REQUESTS DS 0C                                                   
* ASSIGN THE TRUE JIRA ISSUE NUMBER TO PANAPT MOVE REQUESTS WHICH WERE          
* PROMOTED TO PROD USING JIRA ISSUE "EMERGENCY".                                
*                                                                               
         DC    CL6'046017',CL16'DSPTK-155'                                      
         DC    CL6'046935',CL16'OPT-1757'                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
O_DES_RECORD DS 0C                                                              
*                                                                               
           DC  C'="'                                                            
O_DESNUMB  DS  CL6                 MOVE REQUEST NUMBER                          
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
           DC  C'=HYPERLINK("https://jira.mediaocean.com/browse/'               
O_DESSRREQ DS  CL16                PANAPT "SERVICE REQUEST"                     
           DC  C'","'                                                           
O_JIRA#    DS  CL16                JIRA TICKET NUMBER                           
           DC  CL2' '                                                           
O_JIRA#_X  EQU *                                                                
           DC  X'05'                                                            
*                                                                               
O_EMERGENCY_FLAG DS C              'Y' = JIRA TICKET # IS "EMERGENCY"           
           DC  X'05'                                                            
*                                                                               
O_DESADDID DS  CL8                 USER ADD ID                                  
           DC  X'05'                                                            
*                                                                               
           DC  C'="'                                                            
O_DESDESCR DS  CL55                MR DESCRIPTION                               
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
O_DATE_TO_PROD DS CL10             YYYY-MM-DD                                   
           DC  X'05'                                                            
*                                                                               
COLUMNS_FOR_AUDIT_REPORT EQU *-O_DES_RECORD                                     
*                                                                               
* THE COLUMNS BELOW ARE ONLY INFORMATIONAL. THEY MAY BE OF INTEREST             
* INTERNALLY, BUT NOT TO THE AUDITORS.                                          
*                                                                               
O_DESSTAT  DS  CL6                 CURRENT STATUS OF MOVE REQUEST               
           DC  X'05'                                                            
*                                                                               
O_DESCLSN  DS  CL4                 MR CURRENT LEVEL SHORT NAME                  
           DC  X'05'                                                            
*                                                                               
O_DESMTYPE DS  CL1                 MOVE TYPE                                    
           DC  X'05'                                                            
*                                                                               
O_JIRA_PROJECT DS CL8              JIRA PROJECT CODE                            
O_JIRA_PROJECT_X EQU *                                                          
           DC  X'05'                                                            
*                                                                               
O_DES#MEM  DS  CL8                 # OF MEMBER RECORDS                          
           DC  X'05'                                                            
*                                                                               
O_DESNAME  DS  CL16                MOVE REQUEST NAME                            
           DC  X'05'                                                            
*                                                                               
O_DESFMVDA DS  CL10                MR FINAL MOVE DATE (YYYY-MM-DD)              
           DC  X'05'                                                            
O_DESNXMDA DS  CL10                MR NEXT MOVE DATE (YYYY-MM-DD)               
           DC  X'05'                                                            
*                                                                               
O_DESESTOP DS  CL4                 EARLY STOP LEVEL (SPACES = NO STOP)          
           DC  X'05'                                                            
*                                                                               
O_DESADDDA DS  CL10                ADD DATE (YYYY-MM-DD)                        
           DC  X'05'                                                            
*                                                                               
           DC  C'="'                                                            
O_DESADDTI DS  0CL8                ADD TIME                                     
O_DESADDHH DS  CL2                  +ADD HOUR                                   
           DC  C':'                                                             
O_DESADDMN DS  CL2                  +ADD MINUTE                                 
           DC  C':'                                                             
O_DESADDSS DS  CL2                  +ADD SECOND                                 
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
O_DESUPDID DS  CL8                 UPDATE USER ID                               
           DC  X'05'                                                            
*                                                                               
O_DESUPDDA DS  CL10                DATE OF LAST UPDATE (YYYY-MM-DD)             
           DC  X'05'                                                            
*                                                                               
           DC  C'="'                                                            
O_DESUPDTI DS  0CL8                TIME OF LAST UPDATE                          
O_DESUPDHH DS  CL2                 +LAST UPDATE HOUR                            
           DC  C':'                                                             
O_DESUPDMI DS  CL2                 +LAST UPDATE MINUTE                          
           DC  C':'                                                             
O_DESUPDSS DS  CL2                 +LAST UPDATE SECOND                          
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
O_DESMRCSN_STGE DS  CL4            STGE COMPLETED LEVEL SNAME                   
           DC  X'05'                                                            
O_DESMRCDS_STGE DS  CL10           STGE COMPLETED DATE SCHED YYYY-MM-DD         
           DC  X'05'                                                            
O_DESMRCDM_STGE DS  CL10           STGE COMPLETED DATE MOVED YYYY-MM-DD         
           DC  X'05'                                                            
           DC  C'="'                                                            
O_DESMRCTM_STGE DS  0CL8           STGE COMPLETED TIME MOVED                    
O_DESMRCHH_STGE DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCMM_STGE DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCSS_STGE DS  CL2                                                         
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
O_DESMRCSN_PRKS DS  CL4            PRKS COMPLETED LEVEL SNAME                   
           DC  X'05'                                                            
O_DESMRCDS_PRKS DS  CL10           PRKS COMPLETED DATE SCHED YYYY-MM-DD         
           DC  X'05'                                                            
O_DESMRCDM_PRKS DS  CL10           PRKS COMPLETED DATE MOVED YYYY-MM-DD         
           DC  X'05'                                                            
           DC  C'="'                                                            
O_DESMRCTM_PRKS DS  0CL8           PRKS COMPLETED TIME MOVED                    
O_DESMRCHH_PRKS DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCMM_PRKS DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCSS_PRKS DS  CL2                                                         
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
O_DESMRCSN_PROD DS  CL4            PROD COMPLETED LEVEL SNAME                   
           DC  X'05'                                                            
O_DESMRCDS_PROD DS  CL10           PROD COMPLETED DATE SCHED YYYY-MM-DD         
           DC  X'05'                                                            
O_DESMRCDM_PROD DS  CL10           PROD COMPLETED DATE MOVED YYYY-MM-DD         
           DC  X'05'                                                            
           DC  C'="'                                                            
O_DESMRCTM_PROD DS  0CL8           PROD COMPLETED TIME MOVED                    
O_DESMRCHH_PROD DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCMM_PROD DS  CL2                                                         
                DC  C':'                                                        
O_DESMRCSS_PROD DS  CL2                                                         
           DC  C'"'                                                             
           DC  X'05'                                                            
*                                                                               
         DC    (LRECLQ-(*-O_DES_RECORD))C' '                                    
         EJECT                                                                  
SEP_REC  DS    0H                                                               
         DC    C'SEP='             EXCEL DELIMITER                              
         DC    X'05'               TAB                                          
         DC    (LRECLQ-(*-SEP_REC))C' '                                         
*                                                                               
HEADINGS DS    0H                                                               
*                                                                               
         DC    C'MR#',X'05'                                                     
         DC    C'JIRA TICKET',X'05'                                             
         DC    C'EMERGENCY PROMOTION',X'05'                                     
         DC    C'USER ADD ID',X'05'                                             
         DC    C'DESCRIPTION',X'05'                                             
         DC    C'DATE TO PRODUCTION',X'05'                                      
HEADINGS_FOR_AUDIT_REPORT EQU *-HEADINGS                                        
*                                                                               
* THE COLUMNS BELOW ARE ONLY INFORMATIONAL. THEY MAY BE OF INTEREST             
* INTERNALLY, BUT NOT TO THE AUDITORS.                                          
*                                                                               
         DC    C'STATUS',X'05'                                                  
         DC    C'LEVEL',X'05'                                                   
         DC    C'MOVE TYPE',X'05'                                               
         DC    C'JIRA PROJECT',X'05'                                            
         DC    C'# OF MEMBERS',X'05'                                            
         DC    C'MR NAME',X'05'                                                 
         DC    C'FINAL MOVE DATE',X'05'                                         
         DC    C'NEXT MOVE DATE',X'05'                                          
         DC    C'EARLY STOP LEVEL',X'05'                                        
         DC    C'ADD DATE',X'05'                                                
         DC    C'ADD TIME',X'05'                                                
         DC    C'UPDATE USER ID',X'05'                                          
         DC    C'DATE OF LAST UPDATE',X'05'                                     
         DC    C'TIME OF LAST UPDATE',X'05'                                     
         DC    C'STGE LEVEL INFO',X'05'                                         
         DC    C'STGE SCHEDULED MOVE DATE',X'05'                                
         DC    C'STGE COMPLETED MOVE DATE',X'05'                                
         DC    C'STGE COMPLETED MOVE TIME',X'05'                                
         DC    C'PRKS LEVEL INFO',X'05'                                         
         DC    C'PRKS SCHEDULED MOVE DATE',X'05'                                
         DC    C'PRKS COMPLETED MOVE DATE',X'05'                                
         DC    C'PRKS COMPLETED MOVE TIME',X'05'                                
         DC    C'PROD LEVEL INFO',X'05'                                         
         DC    C'PROD SCHEDULED MOVE DATE',X'05'                                
         DC    C'PROD COMPLETED MOVE DATE',X'05'                                
         DC    C'PROD COMPLETED MOVE TIME',X'05'                                
*                                                                               
         DC    (LRECLQ-(*-HEADINGS))C' '                                        
         EJECT                                                                  
         SPACE 3                                                                
PAPTREC  DC    2000C' '            EXTRACTED PANAPT RECORD                      
*                                                                               
EMERGENCYD DSECT                                                                
EMER_MR# DS    CL6                 MOVE REQUEST NUMBER                          
EMER_TKT DS    CL16                JIRA TICKET NUMBER                           
EMER_LQ  EQU *-EMERGENCYD                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDPAPTAUD 10/07/19'                                      
         END                                                                    
