*          DATA SET ACBRA02    AT LEVEL 046 AS OF 03/30/20                      
*PHASE T62402A                                                                  
ACBRA02  TITLE '- ACCOUNTING/BRA SERVER SUPPORT ROUTINES 2'                     
*                                                                               
* LEVEL CHANGE COMMENTS                                                         
* ---------------------                                                         
* UK LEVELS                                                                     
* ---------                                                                     
* NSHE 001 04JAN08 RELEVEL AS ALL PREVIOUS CODE REMOVED                         
* NSHE 002 31JAN08 MERGE ROUTINE 03 TO ROUTINES 02                              
* TKLU             <BR11874D> SECURE ORDER UPLOAD BY CODING VALEST              
* NSHE 003 13FEB08 STOP TSAR ROUTINE FROM GETTING STORAGE EVERYTIME             
* TKLU 004 08FEB08 <LO01-6456> ETYPE APPLICATION LOCKS                          
* TKLU 005 01APR08 <UKCR00010225> Checked for locked (expense) account          
* NSHE 006 30JUN08 Profile no job input to time allowed                         
* NSHE 007 21AUG08 Remove time routines to overlay for time                     
* NSHE 008 23OCT08 Change job backup and non client approver structure          
* JFOS 009 23MAR09 <LO01-7636> New routine for KSV accounts                     
* JFOS     21APR09 <LO01-7636> New routine for Due date formula                 
* NSHE 010 14MAY09 Changes to approver structure                                
* SMAN 011 18JUN09 <LO01-8837> Scheme Limit List / change GAPLST                
* MPEN 011 08JUN09 <LO01-8945> Pass no job level entry setting                  
* MPEN     23JUN09 <LO01-9047> Pass billable only setting                       
* NSHE 012 19SEP09 Rewrite of GAPLST logic for approvals by off/media           
* SMAN 013 28SEP09 <BR14638D> Fix to back up approval search in GAPLST          
* SMAN 014 30SEP09 <BR14644D> Fix to calls to GAPLST using filter               
* NSHE 015 04OCT09 Add new expense passives to gaplst table                     
* NSHE 016 28OCT09 Use TSAR for GAPLST                                          
* NSHE 017 21JAN10 Allow access only for GAPLST                                 
* NSHE 018 26JAN10 Correct APAPTBL for expenses                                 
* NRAK 019 03MAR10 <LO01-9305> Allow GAPLST to concatenate tables               
* SMAN 020 14MAY10 <BR15980D> Increase buffering for Est. upload                
* NRAK 022 14JUL10 <lo01-9305> support 1N 'view' in limlist call                
* NRAK 022         <UKCR00028727> include office when converting lidel          
* NSHE 023 13AUG10 Fix for backup approvers and office level approvers          
* NRAK 024 10sep10 Relink for new BRAWRK (TD#SEQ/TD#KYLN change)                
* NSHE 025 27JUL10 Add creditor and etype to GAPLST module                      
* MPEN 026 09FEB11 Fix for 1R exception in GAPLST                               
* NSHE 028 18FEB11 Fix GAPLST parameter                                         
* MPEN 029 18FEB11 PR001545 change upload process so xdata is 1st el            
* NSHE 030 13MAY11 UKCR32154 Change GAPTABD to have a sequence for              
*                  backup approvers                                             
* NRAK 031 05JUL11 <OT67360L> LOOPING BUG IN GAPLST                             
* JFOS 032 03AUG11 <BR18352D> ENSURE BACKUP APPR CODE READS SQ 0 APPREC         
* NRAK 033 09DEC10 ADD GAPLST PARAM FOR CALLING MODULE                          
* NRAK     10JAN10 ADD MANY ENTRIES FOR LIST/SEARCH                             
* JFOS       NOV11 GAPLST TWEAKS, DON'T SET DEFAULT ENTRY FOR TIME/EXPS         
* NSHE     07MAR12 ADD OFFICE TO SUPPLIER LIMIT LIST - AMEND GAPLST             
* YNGX     09AUG12 <OT73157L> FIX BACKUP APPROVER TIME SEARCH BUG               
* YNGX     29AUG12 <BR52978L> BUG FIX IN GAPLST:IGNORE DUP IF BY ACCESS         
* JFOS 034 30MAY13 <DSBO-30> AMEND NASTY HARD-CODED MTSAR INIT FOR ESTS         
* NSHE 035 06DEC13 <DSRD-35> CHANGE GAPLST TO PUT OUT ACCESS ALL                
*                  ACCOUNTS RECORD FOR TIME AND EXPENSES                        
* JFOS 036 02JUN14 <DSBO-873> STORAGE PROT. ERROR: OC BECOMES CLI               
* MPEN 037 05SEP14 <DSRD-2733> ENSURE SCHEME DEFAULT ACCESS POPULATED           
* MPEN 038 19Dec14 <DSRD-6423> Expenditure type GAPLST changes                  
* NSHE 039 01Apr15 <DSRD-6620> Ensure GAPLST doesn't set access to all          
* NSHE              if entries exist - this is EU only behaviour                
* MPEN 040 20Oct15 <DSRD-9147> Different error message for limlist              
* MPEN 041 07Sep16 <DSRD-13161> Department/office columns                       
* NSHE 042 22Jun17 <DSRD-16105> Allow to pass PIN to GAPLST                     
* MPEN 043 19Mar18 <DSRD-18367> Add new workcode GAPLST call                    
* MPEN 044 23Oct18 <DSRD-20447> Timeoff approver changes                        
*                               Fixes for workcode GAPLST call                  
* MPEN 045 16Jul19 <DSRD-22535> Relink for new DSECT length                     
* NSHE 046 27Mar19 <DSRD-25924> Improve error messages for GAPLST               
*                                                                               
* US LEVELS                                                                     
* ---------                                                                     
* JSHA 004 28SEP07 ALL UK LEVELS UP TO 5                                        
* JSHA 005 29NOV07 US/UK MERGE FROM LEVELS 6 AND 7                              
* JSHA 007 18Mar08 All routines moved from 03 to 02                             
* JSHA 008 31Mar08 Made GETTSN re-entrant-so initializing is UK only            
* JSHA             For US all initializing is done b4 GETTSN loop               
* JSHA 010 25Jul08 Added SY & SW as vailid ledgers for Expense Claims           
* RGUP 011 25Aug08 Added SB as a valid ledger for Expense Claims                
* RGUP 012 29Aug08 Added code to pass back exp job status in CL#EJOB            
* JSHA 013 12Dec08 US/UK Merge for UK Level 7                                   
* JSHA             UK GAPLST changes and Expense validation changes             
* JSHA 014 28Jan09 GAPLST changes for LIMIT HR search for DDB                   
* JSHA 015 01May09 Fixed CSTPRF to make building 1R soft                        
* JSHA 016 29Jun09 Merge for UK levels 9/10                                     
*                                                                               
ACBRA02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**T62402,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    RE,ROU2RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                ** ROUTINE TABLE **                          
         DC    AL2(SETFAC-ACBRA02),AL2(0)                                       
         DC    AL2(GAPLST-ACBRA02),AL2(GAPWRKL)                                 
         DC    AL2(MTSAR-ACBRA02),AL2(0)                                        
         DC    AL2(CSTPRF-ACBRA02),AL2(0)                                       
         DC    AL2(CLRIO-ACBRA02),AL2(0)                                        
         DC    AL2(GOATSR-ACBRA02),AL2(GOAWRKL)                                 
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* SET 2 BYTE CODE FOR FACPAK STATISTICS                                         
***********************************************************************         
         DS    0H                                                               
SETFAC   J     *+12                                                             
         DC    CL8'*SETFAC*'                                                    
         LR    RB,RF                                                            
         USING SETFAC,RB                                                        
         STH   R1,HALF1                                                         
         OC    HALF1,HALF1                                                      
         JZ    EXITY                                                            
         GOTO1 VGETFACT,DMCB,(X'80',HALF1),F#SETUSR                             
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Get list of 1R/SJ/1N accounts a person has approval or access rights*         
* - Sets error in full2 and CC=NEQ if empty/full                      *         
* Ntry - byte1 - type of data required                                *         
*        addr1 - Tsar buffer                                          *         
*        byte2 - type of call - approver, backup approver, limitlist  *         
*        addr2 - filter value                                         *         
*        byte3 - calling program type - status, access or date view   *         
*        addr3 - additional parameter                                 *         
*        byte4 - module equate - time, expenses etc.                  *         
*        addr4 - passes PIN if you want to ignore connected user      *         
***********************************************************************         
         DS    0H                                                               
GAPLST   J     *+12                                                             
         DC    CL8'*GAPLST*'                                                    
         LR    RB,RF                                                            
         USING GAPLST,RB                                                        
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA                                                       
         USING GAPWRKD,RC                                                       
         L     R4,0(R1)                                                         
         MVC   TSARABUF,0(R4)                                                   
         MVC   GAPLDAT1,0(R1)                                                   
*                                                                               
         L     R4,4(R1)                                                         
         MVC   GAPLFILT,0(R4)                                                   
         MVC   GAPLCALT,4(R1)                                                   
*                                                                               
         L     R4,8(R1)                                                         
         MVC   GAPLPARM,0(R4)                                                   
         MVC   GAPLTYPE,8(R1)                                                   
*                                                                               
         ICM   R4,7,13(R1)         Do we have fourth parm                       
         JZ    *+10                No                                           
         MVC   GAPPIN,0(R4)        Yes                                          
         MVC   GAPLMODL,12(R1)                                                  
         XC    GLRSTAS1(2),GLRSTAS1     1 AND 2                                 
         XC    GSEQUNCE,GSEQUNCE                                                
         XC    GSVLDG,GSVLDG                                                    
         XC    BYTE2,BYTE2                                                      
         XC    ATSRERRS,ATSRERRS                                                
         XC    GAPFLTLN,GAPFLTLN                                                
         XC    GAPACTLN,GAPACTLN                                                
         USING GAPTABD,R4                                                       
         LA    R4,GAPAREA                                                       
         XC    GAPLTSAR,GAPLTSAR                                                
         TM    GAPLPARM,GAPLPADD                                                
         JZ    GAPL004                                                          
         LA    RF,TSARABUF                                                      
         USING TSARD,RF                                                         
         MVC   TSRNUM(L'TSRNUM+L'TSRNUMX),TSPRECN    TSI2MANY SET               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPLTSAR,  X        
               TSARABUF           READ HIGHEST RECORD BY NUMBER                 
                                                                                
GAPL004  CLI   GAPLCALT,GAPLLIML   ARE WE GETTING LIST FROM LIMIT LIST          
         JNE   GAPL010                                                          
*                                                                               
         USING LLSRECD,R2                                                       
         LA    R2,IOKEY            Read limit list record                       
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         OC    GAPPIN,GAPPIN                                                    
         JZ    *+10                                                             
         MVC   LLSKPIDB,GAPPIN                                                  
         MVC   CSVKEY1,LLSKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL005                                                          
         DC    H'0'                                                             
* FOR FIRST LLSREC READ ONLY, MANAGE 'DEFAULT ACCESS' SETTINGS                  
* IF NO HIT, ACCRUL WILL DERIVE FROM COMPANY/OFFICE RECORDS                     
* IF HIT, DEFAULT TO 'ALL ON' AND LOOK FOR RSTELS LATER TO MODIFY               
GAPL005  DS    0H                                                               
         CLC   LLSKEY(LLSKGRP-LLSRECD),CSVKEY1                                  
         JNE   GAPL006                                                          
         MVI   GLRSTAS1,RSTAJOBS+RSTAMED+RSTAETYP+RSTA1NAC+RSTASTAF+RST+        
               ASCHM+RSTAWC                                                     
         MVI   GLRSTAS2,RSTASUPP                                                
         J     GAPL034                                                          
*                                                                               
GAPL006  DS    0H                                                               
         GOTOR ACCRUL                                                           
         J     GAPL180                                                          
                                                                                
GAPL007  L     R2,AIO3                                                          
         MVC   CSVKEY1,0(R2)                                                    
         MVC   IOKEY,0(R2)                                                      
         LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL008                                                          
         DC    H'0'                                                             
*                                                                               
GAPL008  DS    0H                                                               
         CLI   CSVKEY1+(GLSKTYP-GLSRECD),GLSKTYPQ                               
         JNE   GAPL009                                                          
         CLI   CSVKEY1+(GLSKSUB-GLSRECD),GLSKSUBQ                               
         JNE   GAPL009                                                          
         CLC   IOKEY(GLSKSEQ-GLSRECD),CSVKEY1                                   
         JNE   GAPL180                                                          
         J     GAPL034                                                          
*                                                                               
GAPL009  CLC   LLSKEY(LLSKGRP-LLSRECD),CSVKEY1                                  
         JNE   GAPL180                                                          
         J     GAPL034                                                          
                                                                                
GAPL010  CLI   GAPLCALT,GAPLBKAP   BACK UP APPROVER                             
         JNE   GAPL028                                                          
*                                                                               
         USING PIDRECD,R2                                                       
         LA    R2,IOKEY            READ APPROVER RECORD                         
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,CCTPID                                                   
         OC    GAPPIN,GAPPIN                                                    
         JZ    *+10                                                             
         MVC   PIDKPID,GAPPIN                                                   
         MVI   PIDKSTYP,PIDKBAPQ                                                
         MVC   CSVKEY1,PIDKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL020                                                          
         DC    H'0'                                                             
                                                                                
GAPL016  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GAPL020  CLC   PIDKEY(PIDKPIDO-PIDRECD),CSVKEY1                                 
         JNE   GAPL180                                                          
         MVC   GAPLBPID,PIDKPIDO                                                
GAPL022  MVC   CSVKEY1,PIDKEY      SAVE FOR REBUILDING LATER                    
         CLI   GAPLMODL,QEXPCLM       EXPENSE ?                                 
         JNE   GAPL024                                                          
         TM    PIDKAPPL,LIDLEXPN                                                
         JZ    GAPL016                                                          
         J     GAPL026                                                          
GAPL024  CLI   GAPLMODL,QTIME         TIMESHEETS ?                              
         JNE   GAPL026                                                          
         TM    PIDKAPPL,LIDLTIME                                                
         JZ    GAPL016                                                          
                                                                                
GAPL026  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         USING APPRECD,R2                                                       
         L     R2,AIO3             PASSIV MAY NOT POINT TO SEQ 0 APPREC         
         MVI   APPKSEQ,X'FF'       ...SO FORCE READ LOOP TO GET IT NOW          
         J     GAPL037                                                          
                                                                                
GAPL028  CLI   GAPLCALT,GAPLAPPR                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOKEY            READ APPROVER RECORD                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,CCTPID                                                  
         OC    GAPPIN,GAPPIN                                                    
         JZ    *+10                                                             
         MVC   APPKPIDB,GAPPIN                                                  
         MVC   CSVKEY1,APPKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   APPKEY(APPKSEQ-APPRECD),CSVKEY1                                  
         JNE   GAPL180                                                          
         CLI   GAPLDAT1,GAPTT9Q                                                 
         JNE   GAPL034                                                          
         TM    APPKSTAT,APPSFIND+APPSFINA                                       
         JZ    GAPL034                                                          
         XC    GAPTDAT1(GAPTLNQ),GAPTDAT1                                       
         MVI   GAPTDAT1,GAPTT9Q    Set type of GAPLST table entry               
         MVI   GAPTSTA,GAPTSIQ+GAPTSMQ  Set main entry and include              
         MVC   GAPTCODE,SPACES     Clear account to spaces                      
         MVI   GAPTPASS,DPAPAEXF                                                
         MVI   GAPTLVL,GAPTSL0                                                  
         MVC   GAPTBKUP,GAPLBPID   Save pid of approver                         
         CLI   GAPLTYPE,GAPLTSTA   Are we looking by date or status             
         JNE   *+8                 By date or access                            
         MVI   GAPTAPPL,EXSPLFN1   By status                                    
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE2,1             Set we added an entry                        
         J     GAPL034                                                          
                                                                                
GAPL030  LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL032                                                          
         DC    H'0'                                                             
                                                                                
GAPL032  DS    0H                                                               
         CLC   APPKEY(APPKSEQ-APPRECD),CSVKEY1                                  
         JNE   GAPL180                                                          
                                                                                
GAPL034  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,APPRFST                                                       
                                                                                
GAPL036  CLI   RSTEL,0                                                          
         JNE   GAPL038                                                          
         CLI   GAPLCALT,GAPLLIML   USING LIMIT LIST                             
         JE    GAPL007             YES                                          
         CLI   GAPLCALT,GAPLBKAP   BACK UP APPROVER                             
         JNE   GAPL030             NO - NORMAL APPROVAL                         
* WE GET ONE BACKUP POINTER PER APPROVER, EVEN IF APPROVER RECORD IS            
* SPLIT OVER MULTIPLE PHYSICAL RECORDS. WE LOOK FOR THE OTHERS HERE             
* BEFORE READING THE NEXT GAPLST ENTRY.                                         
GAPL037  XC    IOKEY,IOKEY                                                      
         L     R2,AIO3                                                          
         MVC   IOKEY(L'APPKEY),APPKEY                                           
         LA    R2,IOKEY                                                         
         LLC   RE,APPKSEQ                                                       
         LA    RE,1(RE)                                                         
         STC   RE,APPKSEQ                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    GAPL034             FOUND ONE, READ IT AND COME BACK.            
* NOT FOUND, RESTORE RDHI/SQ SEQUENCE, THEN GET NEXT GAPLST                     
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'PIDKEY),CSVKEY1                                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         J     GAPL016             BACK UP APPROVAL                             
                                                                                
GAPL038  CLI   RSTEL,LIDELQ                                                     
         JE    GAPL042                                                          
         CLI   RSTEL,RSTELQ                                                     
         JE    GAPL041                                                          
                                                                                
GAPL040  XR    R0,R0                                                            
         IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         J     GAPL036                                                          
*                                                                               
GAPL041  DS    0H                                                               
         CLI   RSTLN,RSTLN3Q                                                    
         JL    GAPL040                                                          
         OC    GLRSTAS1(2),GLRSTAS1                                             
         JZ    GAPL040                                                          
         TM    RSTACST1,RSTAJOBS                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAJOBS    CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTAMED                                                 
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAMED     CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTAETYP                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAETYP    CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTA1NAC                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTA1NAC    CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTASTAF                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTASTAF    CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTAWC                                                  
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAWC      CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST1,RSTASCHM                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTASCHM    CLEAR TO SUPPRESS 'ALL ACCESS'           
         TM    RSTACST2,RSTASUPP                                                
         JZ    *+8                                                              
         NI    GLRSTAS2,FF-RSTASUPP    CLEAR TO SUPPRESS 'ALL ACCESS'           
         J     GAPL040                                                          
*                                                                               
         USING LIDELD,R3                                                        
GAPL042  LA    R7,GPRTTAB                                                       
         USING GPRTTABD,R7                                                      
GAPL044  CLI   GPRTDAT,X'FF'                                                    
         JE    GAPL040                                                          
         CLC   GAPLDAT1,GPRTDAT                                                 
         JNE   GAPL046                                                          
         CLC   GAPLCALT,GPRTTYP                                                 
         JNE   GAPL046                                                          
         CLC   LIDTYPE,GPRTLID                                                  
         JNE   GAPL046                                                          
         XR    R5,R5                                                            
         IC    R5,LIDLN                                                         
         LA    R5,LIDELD(R5)       R5=A(End of element)                         
         ST    R5,GENDELE                                                       
         LA    R6,LIDDATA          R6=A(Start of data on LIDELD)                
         J     GAPL048                                                          
GAPL046  LA    R7,GPRTTABL(R7)                                                  
         J     GAPL044                                                          
                                                                                
AC       USING LIDDATA,R6                                                       
                                                                                
GAPL048  CLM   R6,15,GENDELE       End of element?                              
         JNL   GAPL040             Yes                                          
*                                                                               
         LA    R2,APAPTBL                                                       
         USING APAPTBLD,R2                                                      
         XC    GAPTDAT1(GAPTLNQ),GAPTDAT1                                       
                                                                                
GAPL050  CLC   GPRTLDG,APAPLDG                                                  
         JNE   GAPL124                                                          
                                                                                
         CLC   APAPLDG,=C'XX'                                                   
         JE    GAPL070                                                          
         MVI   GAPFLTLN,0                                                       
         MVI   GAPACTLN,0                                                       
         XR    R5,R5                                                            
         ICM   R5,1,APAPLUNL       Is the ledger on the LIDELD                  
         JZ    GAPL051             No                                           
         AR    R5,R6               Yes                                          
         CLC   GAPLFILT,SPACES     Do we have a filter                          
         JNH   GAPL050A            No                                           
         CLC   GAPLFILT(L'ACTKUNT+L'ACTKLDG),0(R5) Do ledgers match             
         JNE   GAPL124             No - go to next table entry                  
GAPL050A CLC   GSVLDGRD,0(R5)                                                   
         JE    GAPL054                                                          
         MVC   GSVLDGRD,0(R5)                                                   
         J     GAPL053                                                          
                                                                                
GAPL051  CLC   GAPLFILT,SPACES     Have we read with a filter                   
         JNH   GAPL052             No                                           
         CLC   GPRTLDG,GAPLFILT    Does the filter ledger match                 
         JNE   GAPL040             No - skip this element                       
GAPL052  CLC   GSVLDG,APAPLDG                                                   
         JE    GAPL054                                                          
         MVC   GSVLDGRD,APAPLDG                                                 
GAPL053  MVC   GSVLDG,APAPLDG                                                   
         MVC   GSVIOKEY,IOKEY                                                   
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,GSVLDGRD                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         MVC   IOKEY,GSVIOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL054                                                          
         DC    H'0'                                                             
                                                                                
GAPL054  CLC   GAPLFILT,SPACES     Have we read with a filter                   
         JNH   GAPL070             No                                           
         CLC   GSVLDGRD,GAPLFILT   Does the filter ledger match                 
         JNE   GAPL124             No - skip table entry                        
         LA    R5,GAPLRACT                                                      
         CLC   0(L'ACTKACT,R5),SPACES                                           
         JNH   GAPL070                                                          
         LLC   R1,LDGAL1                                                        
         CHI   R1,L'ACTKACT                                                     
         JE    GAPL058                                                          
         AR    R1,R5                                                            
GAPL056  CLI   0(R1),C' '                                                       
         JH    GAPL060                                                          
GAPL058  MVC   GAPFLTLN,LDGAL1     Set filter is level 1 length                 
         J     GAPL070                                                          
                                                                                
GAPL060  LLC   R1,LDGAL2                                                        
         CHI   R1,L'ACTKACT                                                     
         JE    GAPL062                                                          
         AR    R1,R5                                                            
         CLI   0(R1),C' '                                                       
         JH    GAPL064                                                          
GAPL062  MVC   GAPFLTLN,LDGAL2     Set filter is level 2 length                 
         J     GAPL070                                                          
                                                                                
GAPL064  LLC   R1,LDGAL3                                                        
         CHI   R1,L'ACTKACT                                                     
         JE    GAPL066                                                          
         AR    R1,R5                                                            
         CLI   0(R1),C' '                                                       
         JH    GAPL068                                                          
GAPL066  MVC   GAPFLTLN,LDGAL3     Set filter is level 3 length                 
         J     GAPL070                                                          
                                                                                
GAPL068  MVC   GAPFLTLN,LDGAL4     Set filter is level 4 length                 
*                                                                               
GAPL070  DS    0H                                                               
         CLC   GAPLMODL,APAPMOD       Does the module match                     
         JNE   GAPL124             No - get next table entry                    
         CLI   APAPDTYP,0          Do we need to check request type             
         JE    GAPL071             No                                           
         CLC   GAPLDAT1,APAPDTYP   Yes - does it match table entry              
         JNE   GAPL124             No - get next table entry                    
*                                                                               
GAPL071  CLC   LIDTYPE,APAPLTYP    Does the LIDELD type match                   
         JNE   GAPL124             No                                           
*                                                                               
         CLC   APAPLDG,=C'XX'                                                   
         JE    GAPL086                                                          
*                                                                               
         LLC   R5,APAPLACT         R5=displacement to account field             
         AR    R5,R6               R5=A(Account code in LIDELD)                 
         XR    R0,R0                                                            
         MVI   GAPLEVEL,GAPTSL0                                                 
         CLI   0(R5),C' '          Have we got an account                       
         JE    GAPL084             No - could be global entry                   
         LLC   R1,LDGAL1                                                        
         CHI   R1,L'ACTKACT        Is this a one level ledger                   
         JE    GAPL072             Yes                                          
         AR    R1,R5                                                            
         CLI   0(R1),C' '          Anything passed level 1                      
         JH    GAPL074             Yes                                          
GAPL072  MVC   GAPACTLN,LDGAL1     Set account is level 1 length                
         MVI   GAPLEVEL,GAPTSL1                                                 
         J     GAPL084                                                          
                                                                                
GAPL074  LLC   R1,LDGAL2           Try at level 2                               
         CHI   R1,L'ACTKACT                                                     
         JE    GAPL076                                                          
         AR    R1,R5                                                            
         CLI   0(R1),C' '                                                       
         JH    GAPL078                                                          
GAPL076  MVC   GAPACTLN,LDGAL2     Set account is level 2 length                
         MVI   GAPLEVEL,GAPTSL2                                                 
         J     GAPL084                                                          
                                                                                
GAPL078  LLC   R1,LDGAL3                                                        
         CHI   R1,L'ACTKACT                                                     
         JE    GAPL080                                                          
         AR    R1,R5                                                            
         CLI   0(R1),C' '                                                       
         JH    GAPL082                                                          
GAPL080  MVC   GAPACTLN,LDGAL3     Set account is level 3 length                
         MVI   GAPLEVEL,GAPTSL3                                                 
         J     GAPL084                                                          
                                                                                
GAPL082  MVC   GAPACTLN,LDGAL4     Set account is level 4 length                
         MVI   GAPLEVEL,GAPTSL4                                                 
*                                                                               
GAPL084  OC    GAPFLTLN,GAPFLTLN   Did we have a filter                         
         JZ    GAPL086             No                                           
         LLC   RF,GAPFLTLN         YES - SPOOF LIDEL TO CREATE GAPLST           
         LLC   R0,GAPACTLN               ENTRY AT FILTER'S LEVEL                
         CR    R0,RF                                                            
         JH    *+6                                                              
         LR    RF,R0                                                            
         SHI   RF,1                Compare filter to entry in LIDELD            
         BASR  R1,0                                                             
         CLC   0(0,R5),GAPLRACT                                                 
*        EXRL  RF,*-6                                                           
         EX    RF,0(R1)                                                         
         JNE   GAPL124             Reject if not matching                       
         LLC   RF,GAPFLTLN                                                      
         CR    R0,RF                                                            
         JNL   GAPL086                                                          
         OI    GAPTSTA,GAPTSMDQ    REMEMBER MODIFIED                            
         MVC   0(L'GAPLRACT,R5),GAPLRACT                                        
         MVC   GAPACTLN,GAPFLTLN                                                
         MVI   GAPLEVEL,GAPTSL1                                                 
         CLC   GAPACTLN,LDGAL1                                                  
         JE    GAPL086                                                          
         MVI   GAPLEVEL,GAPTSL2                                                 
         CLC   GAPACTLN,LDGAL2                                                  
         JE    GAPL086                                                          
         MVI   GAPLEVEL,GAPTSL3                                                 
         CLC   GAPACTLN,LDGAL3                                                  
         JE    GAPL086                                                          
         MVI   GAPLEVEL,GAPTSL4                                                 
                                                                                
GAPL086  DS    0H                                                               
         XR    R1,R1                                                            
         ICM   R1,1,APAPWPP        Are we concerned with WPP expenses           
         JZ    GAPL090                                 workflow                 
         TM    SCPXEL+CPXSTAT6-CPXELD,CPX2LAEI  WPP workflow                    
         NOP   GAPL124                                                          
         EX    R1,*-4                                                           
                                                                                
GAPL090  ICM   R1,1,APAPMED                                                     
         JZ    GAPL092                                                          
         CLC   AC.LIDASJME,SPACES                                               
         NOP   GAPL124                                                          
         EX    R1,*-4                                                           
                                                                                
GAPL092  DS    0H                                                               
*&&UK                                                                           
         MVC   GAPTDAT1,APAPGTYP   Set type of GAPLST table entry               
         BRAS  RE,SETRSTF                                                       
*&&                                                                             
         ICM   R1,1,APAPLST1       Have we got anything to check                
         JZ    GAPL094             status byte 1 with                           
         EX    R1,*+8                                                           
         JZ    GAPL124                                                          
         TM    AC.LIDASTAT,0                                                    
                                                                                
GAPL094  ICM   R1,1,APAPLST2       Have we got anything to check status         
         JZ    GAPL096             byte 2 with                                  
         CLI   APAPLTYP,LIDTAP1R   1R account LIDELD type?                      
         JNE   GAPL095                                                          
         EX    R1,*+8                                                           
         JZ    GAPL124                                                          
         TM    AC.LIDAPDT2,0       Check 1R application byte 2                  
         J     GAPL096                                                          
*                                                                               
GAPL095  EX    R1,*+8                                                           
         JZ    GAPL124                                                          
         TM    AC.LIDASTA2,0                                                    
*                                                                               
* LIDEL MATCHES APAPTBL ENTRY, BUILD GAPLST ENTRY NOW                           
*                                                                               
GAPL096  MVC   GAPTDAT1,APAPGTYP   Set type of GAPLST table entry               
         OI    GAPTSTA,GAPTSIQ+GAPTSMQ  Set main entry and include              
         MVC   GAPTACT,SPACES      Clear account to spaces                      
         CLC   APAPLDG,=C'SJ'      If ledger SJ clear longer area               
         JNE   *+10                                                             
         MVC   GAPTCODE,SPACES                                                  
         MVC   GAPTPASS,APAPPTYP                                                
         MVC   GAPTLVL,GAPLEVEL                                                 
         CLI   GAPLTYPE,GAPLTACC   Are we looking by access                     
         JE    GAPL097             Yes                                          
         MVC   GAPTAPPL,APAPDTE                                                 
         CLI   GAPLTYPE,GAPLTDTE   Are we looking by date or status             
         JE    *+10                By date                                      
         MVC   GAPTAPPL,APAPSTA    By status                                    
                                                                                
GAPL097  LLC   R0,GAPACTLN         R0=Length of account                         
         XR    RE,RE                                                            
         ICM   RE,1,APAPGACT       Are we extracting account code               
         JZ    GAPL098             No                                           
         AR    RE,R4                                                            
         MVC   0(L'ACTKACT,RE),0(R5)                                            
         XR    RE,RE                                                            
         ICM   RE,1,APAPGUNL       Are we extracting unit ledger code           
         JZ    GAPL098             No                                           
         LLC   RF,APAPLUNL                                                      
         AR    RE,R4                                                            
         AR    RF,R6                                                            
         MVC   0(L'GSVLDG,RE),0(RF) Extract unit ledger code                    
                                                                                
GAPL098  XR    RE,RE                                                            
         ICM   RE,1,APAPGOFF       Are we extracting office code                
         JZ    GAPL100             No                                           
         LLC   RF,APAPLOFF                                                      
         AR    RE,R4                                                            
         AR    RF,R6                                                            
         MVC   0(L'LIDLOFF,RE),0(RF) Extract office code                        
         CLC   0(L'LIDLOFF,RF),SPACES                                           
         JH    GAPL100                                                          
         GOTOR GETSJOF             Get offices if client code                   
         LLC   RE,APAPGOFF                                                      
         AR    RE,R4                                                            
         CLC   GAPOFFTB(L'LIDLOFF),SPACES                                       
         JNH   *+10                                                             
         MVC   0(L'LIDLOFF,RE),GAPOFFTB                                         
                                                                                
GAPL100  XR    RE,RE                                                            
         ICM   RE,1,APAPGMED       Is there media or other data                 
         JZ    GAPL104             No                                           
         LLC   RF,APAPLMED         Point to where this data is                  
         AR    RE,R4                                                            
         AR    RF,R6                                                            
         LLC   R5,APAPGLEN         What is the length of this data              
         SHI   R5,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,RE),0(RF)                                                    
         EX    R5,0(R1)                                                         
         CHI   R0,0                Is there any account in entry                
         JE    GAPL102             No                                           
         AHI   R0,L'LIDASJME+L'LIDASJOF  Must have media n office               
         MVI   GAPTLVL,GAPTSL7                                                  
         CLC   GAPACTLN,LDGAL2     Was account at product level                 
         JNE   *+8                 No                                           
         MVI   GAPTLVL,GAPTSL8     Yes - set correct level                      
         CLC   GAPACTLN,LDGAL3     Was account at job level                     
         JNE   *+12                No                                           
         MVI   GAPTPASS,0          Yes - set no passive as lowest level         
         MVI   GAPTLVL,GAPTSL9     Set correct level                            
         J     GAPL116                                                          
*                                                                               
GAPL102  AHI   R0,L'LIDASJME       We must have media                           
         MVI   GAPTLVL,GAPTSL5     Set correct level                            
         XR    RE,RE                                                            
         ICM   RE,1,APAPGOFF       But do we have office aswell                 
         JZ    GAPL116             No                                           
         AR    RE,R4               Maybe check office field in GAPTAB           
         CLC   0(L'LIDLOFF,RE),SPACES                                           
         JNH   GAPL116             Not present                                  
         AHI   R0,L'LIDASJOF       present                                      
         MVI   GAPTLVL,GAPTSL6     Set correct level                            
         J     GAPL116                                                          
                                                                                
GAPL104  CLC   APAPLDG,=C'SJ'      If ledger SJ add length of office            
         JNE   *+8                                                              
         AHI   R0,L'LIDASJOF       Will always have an office                   
         MVI   GAPTLVL,GAPTSL1                                                  
         CLC   GAPACTLN,LDGAL1     Was account at client level                  
         JNE   *+8                 No                                           
         MVI   GAPTLVL,GAPTSL2     Yes - set correct level                      
         CLC   GAPACTLN,LDGAL2     Was account at product level                 
         JNE   *+8                 No                                           
         MVI   GAPTLVL,GAPTSL3     Yes - set correct level                      
         CLC   GAPACTLN,LDGAL3     Was account at job level                     
         JNE   *+8                 No                                           
         MVI   GAPTLVL,GAPTSL4     Set correct level                            
         CLC   GAPACTLN,LDGAL4     Was account at person level                  
         JNE   *+8                 No                                           
         MVI   GAPTLVL,GAPTSL5     Set correct level                            
*                                                                               
GAPL116  STC   R0,GAPTLEN                                                       
                                                                                
         CLI   GAPLCALT,GAPLBKAP   Back up approver                             
         JNE   GAPL118             No - normal approval                         
         XR    RF,RF                                                            
         LH    RF,GSEQUNCE                                                      
         AHI   RF,1                                                             
         STH   RF,GSEQUNCE                                                      
         MVC   GAPTXXX,GSEQUNCE                                                 
         MVC   GAPTBKUP,GAPLBPID   Save pid of approver                         
                                                                                
GAPL118  GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    GAPL119                                                          
         CLI   GAPLTYPE,GAPLTACC   Are we looking by access                     
         JNE   *+12                No - dump                                    
         TM    ATSRERRS,TSEDUP     We expect dupes                              
         JNZ   GAPL119                                                          
         DC    H'0'                                                             
GAPL119  MVI   BYTE2,1             Set we added an entry                        
*&&US                                                                           
         BRAS  RE,SETRSTF                                                       
*&&                                                                             
         TM    GAPWIND,GAPWOFF                                                  
         JZ    GAPL124                                                          
         LA    R5,GAPOFFTB+L'LIDLOFF                                            
GAPL120  LLC   RE,APAPGOFF         Are we extracting office code                
         AR    RE,R4                                                            
         MVC   0(L'LIDLOFF,RE),0(R5)                                            
                                                                                
         CLI   GAPLCALT,GAPLBKAP   Back up approver                             
         JNE   GAPL122             No - limit list or approver                  
         XR    RF,RF                                                            
         LH    RF,GSEQUNCE                                                      
         AHI   RF,1                                                             
         STH   RF,GSEQUNCE                                                      
         MVC   GAPTXXX,GSEQUNCE                                                 
         MVC   GAPTBKUP,GAPLBPID   Save pid of approver                         
                                                                                
GAPL122  GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,L'LIDLOFF(R5)                                                 
         CLC   0(L'LIDLOFF,R5),SPACES                                           
         JH    GAPL120                                                          
                                                                                
GAPL124  LA    R2,APAPTBLQ(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         JE    GAPL126                                                          
                                                                                
         CLC   GSVLDG,APAPLDG      Same ledger as before?                       
         JNE   GAPL050                                                          
         J     GAPL070                                                          
                                                                                
GAPL126  LLC   RF,LIDITLN                                                       
         AR    R6,RF               Goto next sub element                        
         XC    GAPWIND,GAPWIND                                                  
         J     GAPL048                                                          
         DROP  AC,R7                                                            
*                                                                               
* add 'all access' entries where RSTEL allows, and no entries already           
* found.                                                                        
*                                                                               
GAPL180  CLI   GAPLCALT,GAPLLIML   Limit list only                              
         JNE   GAPL190                                                          
         LA    RE,RQRSTTAB                                                      
GAPL181  CLI   0(RE),0             CHECK IF CALL RELATES TO RSTEL FLAGS         
         JE    GAPL190             NO                                           
         CLC   GAPLDAT1,0(RE)                                                   
         JE    GAPL182                                                          
         LA    RE,L'RQRSTTAB(RE)                                                
         J     GAPL181                                                          
*                                                                               
GAPL182  NC    GLRSTAS1,1(RE)   Kill all flags not relevant to call             
         NC    GLRSTAS2,2(RE)                                                   
         OC    GLRSTAS1(2),GLRSTAS1                                             
         JZ    GAPL190             Nothing left                                 
                                                                                
         TM    GAPLPARM,GAPLACLS   Come from account list/search                
         JNZ   GAPL184             Build empty entry for all access             
         CLI   GAPLMODL,QEXPCLM    test expenses                                
         JE    GAPL190                                                          
         CLI   GAPLMODL,QTIME      or time                                      
         JE    GAPL190             skip default record                          
         CLI   GAPLMODL,QTIMOFF    or timeoff                                   
         JE    GAPL190             skip default record                          
                                                                                
*        Basic 'all access' entry                                               
                                                                                
GAPL184  XC    GAPAREA,GAPAREA                                                  
         MVC   GAPTCODE,SPACES                                                  
         OI    GAPTSTA,GAPTSIQ+GAPTSMQ                                          
         MVI   GAPTLVL,0                                                        
         MVI   GAPTLEN,L'GAPTCODE                                               
                                                                                
*        add for all types we didn't find accounts for                          
                                                                                
         MVI   GAPTDAT1,GAPTT2Q                                                 
         TM    GLRSTAS1,RSTAJOBS                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT1Q                                                 
         TM    GLRSTAS1,RSTASTAF                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT3Q                                                 
         TM    GLRSTAS1,RSTA1NAC                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT4Q                                                 
         TM    GLRSTAS1,RSTASCHM                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT5Q                                                 
         TM    GLRSTAS1,RSTAMED                                                 
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT8Q                                                 
         TM    GLRSTAS2,RSTASUPP                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         MVI   GAPTDAT1,GAPTT7Q                                                 
         TM    GLRSTAS1,RSTAETYP                                                
         JZ    *+8                                                              
         BRAS  RE,GAPL188                                                       
         J     GAPL190                                                          
*                                                                               
GAPL188  DS    0H                                                               
         ST    RE,SAVERE                                                        
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE2,1             Set we added something                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
* now add exception entries to tsar. rereads buffer, builds passive             
* key and reads high for matches.                                               
*                                                                               
GAPL190  XC    GAPAREA,GAPAREA                                                  
         TM    GAPLPARM,GAPLPADD   If concatenating calls                       
         JZ    GAPL195             No                                           
         OC    GAPLTSAR,GAPLTSAR   And we added something last time             
         JZ    GAPL205             No - error                                   
         CLI   BYTE2,1             Did we add an entry this time                
         JNE   GAPL205             No - error                                   
         MVC   GAPAREA,GAPLTSAR    find 1st record in this call                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     GAPL210                                                          
*                                                                               
*  No concatenation                                                             
*                                                                               
GAPL195  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     Is buffer empty                              
         JNZ   GAPL205                                                          
                                                                                
GAPL200  CLI   BYTE2,1             Did we add an entry this time                
         JE    GAPL210             Yes                                          
*                                                                               
*  Common error exit - nothing found                                            
*                                                                               
GAPL205  MVC   FULL2(2),=AL2(AE$NLIML)                                          
         CLI   GAPLCALT,GAPLLIML   Different error message for limlist          
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPEX)  Put out suitable message                
         CLI   GAPLMODL,QEXPCLM       Expense claim                             
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPTM)                                          
         CLI   GAPLMODL,QTIME         Timesheets                                
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPTO)                                          
         CLI   GAPLMODL,QTIMOFF       Time off                                  
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPES)                                          
         CLI   GAPLMODL,QEST          Estimates                                 
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPOR)                                          
         CLI   GAPLMODL,QORD          Orders                                    
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPIN)                                          
         CLI   GAPLMODL,QINV          Invoices                                  
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$MAPJB)                                          
         CLI   GAPLMODL,QJOB          Jobs                                      
         JE    EXITH                                                            
         MVC   FULL2(2),=AL2(AE$LAPPZ)  If don't know module use                
         J     EXITH                        generic message                     
*                                  Add exceptions                               
GAPL210  LA    R4,GAPAREA          Point to start of table/1st rec              
*                                                            this time          
GAPL220  CLI   GAPLCALT,GAPLLIML   USING LIMIT LIST                             
         JE    GAPL600             YES, no exceptions apply                     
                                                                                
         TM    GAPTSTA,GAPTSEQ     ignore exceptions (we've just added          
         JNZ   GAPL490                                        them)             
                                                                                
         OC    GAPTPASS,GAPTPASS   Do we have a passive to check                
         JZ    GAPL490             No                                           
         CLI   GAPTDAT1,GAPTT1Q    1R line manager                              
         JE    GAPL400                                                          
         CLI   GAPTDAT1,GAPTT9Q    1R finance manager                           
         JE    GAPL400                                                          
                                                                                
         CLI   GAPTDAT1,GAPTT2Q    Office/SJ                                    
         JE    GAPL250                                                          
         CLI   GAPTDAT1,GAPTT6Q    Media/office/SJ                              
         JNE   GAPL490                                                          
         CLI   GAPTLVL,GAPTSL9     Have we got a low level entry                
         JE    GAPL490             Yes - ignore                                 
*                                                                               
* SJ exclusions (with/without media)                                            
*                                                                               
NEW      USING GAPTABD,GAPAREA2                                                 
SJ       USING JOBPASD,IOKEY                                                    
GAPL250  XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVC   SJ.JOBPAPPL,GAPTPASS                                             
         MVC   SJ.JOBPCODE,GAPTCODE                                             
         MVI   SJ.JOBPVIEW,JOBPVMED                                             
         CLI   GAPTDAT1,GAPTT6Q    Are dealing with media approver              
         JE    GAPL255             Yes                                          
         MVI   SJ.JOBPVIEW,JOBPVOFF No                                          
GAPL255  MVC   CSVKEY1,SJ.JOBPAS                                                
                                                                                
GAPL260  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL270                                                          
         DC    H'0'                                                             
                                                                                
GAPL265  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL270                                                          
         DC    H'0'                                                             
                                                                                
GAPL270  CLC   CSVKEY1(JOBPCODE-JOBPASD),SJ.JOBPAS                              
         JNE   GAPL300             No other passives                            
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   SJ.JOBPCODE(0),GAPTCODE                                          
         EX    RF,0(RE)                                                         
         JNE   GAPL300                                                          
         LA    RF,GAPPIN                                                        
         OC    GAPPIN,GAPPIN                                                    
         JNZ   *+8                                                              
         LA    RF,CCTPID                                                        
         CLI   GAPLCALT,GAPLBKAP   Back up approver search                      
         JNE   *+8                 No - normal approver                         
         LA    RF,GAPTBKUP                                                      
         CLC   SJ.JOBPPIDB,0(RF)   Exclude same pid                             
         JE    GAPL265                                                          
                                                                                
         XC    NEW.GAPTDAT1(GAPTLNQ),NEW.GAPTDAT1  Add new entry                
         MVI   NEW.GAPTDAT1,GAPTEOT                                             
         CLI   GAPTDAT1,GAPTT6Q    Are dealing with media approver              
         JE    GAPL285             Yes                                          
*                                                                               
*    SJ exceptions (not media approver)                                         
*                                                                               
         MVI   NEW.GAPTLVL,GAPTSL1                                              
         CLI   SJ.JOBPCPJ,C' '     have cli code?                               
         JE    GAPL275             no                                           
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL2                                              
         LLC   RE,PCLILEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPCPJ(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL275                                                          
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL3                                              
         LLC   RE,PPROLEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPCPJ(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL275                                                          
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL4                                              
         LLC   RE,PJOBLEN                                                       
         AR    R0,RE                                                            
GAPL275  CLI   SJ.JOBPCMED,C' '                                                 
         JH    GAPL280                                                          
         CLI   NEW.GAPTLVL,GAPTSL1                                              
         JE    GAPL265               office code but no media. Can't be         
*                                       an exception to anything                
         J     GAPL295               exception found                            
*                                                                               
GAPL280  LLC   RE,GAPTLVL            media plus some sj account                 
         AHI   RE,GAPTSL5                                                       
         STC   RE,NEW.GAPTLVL                                                   
         J     GAPL295                                                          
*                                                                               
* SJ exceptions (media approver)                                                
GAPL285  MVI   NEW.GAPTLVL,GAPTSL6                                              
         CLI   SJ.JOBPJOB,C' '     Do we have an account                        
         JE    GAPL290             No                                           
         MVI   NEW.GAPTLVL,GAPTSL7                                              
         LHI   R0,L'GAPTOFF+L'GAPTMED                                           
         LLC   RE,PCLILEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPJOB(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL295                                                          
         MVI   NEW.GAPTLVL,GAPTSL8                                              
         LHI   R0,L'GAPTOFF+L'GAPTMED                                           
         LLC   RE,PPROLEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPJOB(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL295                                                          
         MVI   NEW.GAPTLVL,GAPTSL9                                              
         LHI   R0,L'GAPTOFF+L'GAPTMED                                           
         LLC   RE,PJOBLEN                                                       
         AR    R0,RE                                                            
         J     GAPL295                                                          
*                                                                               
GAPL290  CLI   SJ.JOBPOFFC,C' '                                                 
         JNH   GAPL265               no office, sj a/c. Can't be                
*                                            exception to anything              
         MVI   NEW.GAPTLVL,GAPTSL6                                              
         LHI   R0,L'GAPTMED+L'GAPTOFF                                           
*                                                                               
* all SJ again                                                                  
*                                                                               
GAPL295  MVC   NEW.GAPTDAT1,GAPTDAT1  Add new entry                             
         MVI   NEW.GAPTSTA,GAPTSEQ                                              
         MVC   NEW.GAPTAPPL,GAPTAPPL                                            
         STC   R0,NEW.GAPTLEN                                                   
         MVC   NEW.GAPTCODE(L'JOBPCODE),SJ.JOBPCODE                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,           +        
               NEW.GAPTDAT1,TSARABUF                                            
         JE    GAPL265                                                          
         CLI   GAPLCALT,GAPLBKAP   Back up approver search                      
         JNE   *+12                No - dump                                    
         TM    ATSRERRS,TSEDUP     We expect dupes as there can be many         
         JNZ   GAPL265             approvers with the same rights               
         DC    H'0'                                                             
                                                                                
GAPL300  CLI   GAPTDAT1,GAPTT6Q    Are dealing with media approver              
         JE    GAPL490             Yes - nothing else to go                     
                                                                                
         CLC   GAPTACC,SPACES      Do we have an entry without a client         
         JH    GAPL355             No                                           
         XC    SJ.JOBPAS,SJ.JOBPAS Read for passive where no office but         
         MVI   SJ.JOBPTYP,JOBPTYPQ               have client                    
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVC   SJ.JOBPAPPL,GAPTPASS                                             
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVC   SJ.JOBPCODE,SPACES                                               
         MVC   CSVKEY1,SJ.JOBPAS                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL310                                                          
         DC    H'0'                                                             
                                                                                
GAPL305  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL310                                                          
         DC    H'0'                                                             
                                                                                
GAPL310  CLC   CSVKEY1(JOBPCPJ-JOBPASD),SJ.JOBPAS                               
         JNE   GAPL355             No other passives                            
         CLI   SJ.JOBPCMED,C' '    Have we got media                            
         JH    *+12                Yes                                          
         CLI   SJ.JOBPCPJ,C' '     Have we got an account                       
         JNH   GAPL305                                                          
         LA    RF,GAPPIN                                                        
         OC    GAPPIN,GAPPIN                                                    
         JNZ   *+8                                                              
         LA    RF,CCTPID                                                        
         CLI   GAPLCALT,GAPLBKAP   If using back up                             
         JNE   *+8                 check actual pid not connected user          
         LA    RF,GAPTBKUP                                                      
         CLC   SJ.JOBPPIDB,0(RF)   Exclude same pid                             
         JE    GAPL305                                                          
         XC    NEW.GAPTDAT1(GAPTLNQ),NEW.GAPTDAT1 Add new entry                 
         MVC   NEW.GAPTDAT1,GAPTDAT1 Add new entry                              
         MVC   NEW.GAPTCOFF,GAPTCOFF                                            
         MVC   NEW.GAPTXXX,GAPTXXX                                              
         MVC   NEW.GAPTACC,SJ.JOBPCPJ                                           
         MVC   NEW.GAPTCMED(L'JOBPCMED),SJ.JOBPCMED                             
         MVI   NEW.GAPTSTA,GAPTSEQ                                              
         MVC   NEW.GAPTAPPL,GAPTAPPL                                            
                                                                                
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL1                                              
         CLI   SJ.JOBPCPJ,C' '     No - check not lowest level                  
         JE    GAPL315             Yes                                          
         MVI   NEW.GAPTLVL,GAPTSL2                                              
         LLC   RE,PCLILEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPCPJ(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL315                                                          
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL3                                              
         LLC   RE,PPROLEN                                                       
         AR    R0,RE                                                            
         LA    RE,SJ.JOBPCPJ(RE)                                                
         CLI   0(RE),C' '                                                       
         JE    GAPL315                                                          
         LHI   R0,L'GAPTOFF                                                     
         MVI   NEW.GAPTLVL,GAPTSL4                                              
         LLC   RE,PJOBLEN                                                       
         AR    R0,RE                                                            
GAPL315  CLI   SJ.JOBPCMED,C' '                                                 
         JH    GAPL320                                                          
         J     GAPL325                                                          
                                                                                
GAPL320  LLC   RE,GAPTLVL                                                       
         AHI   RE,GAPTSL5                                                       
*                                                                               
GAPL325  STC   R0,NEW.GAPTLEN                                                   
         STC   RE,NEW.GAPTLVL                                                   
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,           +        
               NEW.GAPTDAT1,TSARABUF                                            
         JE    GAPL305                                                          
         TM    ATSRERRS,TSEDUP     We expect dupes as there can be many         
         JNZ   GAPL305             approvers with the same rights               
         DC    H'0'                                                             
*                                                                               
GAPL355  LA    R2,JBPSTBL          table of account levels to check             
         USING JBPSTBLD,R2                                                      
GAPL360  XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RE,3,JBPSDIS                                                     
         JZ    GAPL365                                                          
         AR    RE,R9                                                            
         LLC   RF,0(RE)                                                         
GAPL365  LLC   RE,JBPSPOS                                                       
         AR    RE,R4                                                            
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         JH    GAPL375                                                          
GAPL370  LA    R2,JBPSTBLL(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         JNE   GAPL360                                                          
         J     GAPL490                                                          
                                                                                
GAPL375  XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUXCPY                                                
         MVC   SJ.JOBPAPPL,GAPTPASS                                             
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         XR    RE,RE                                                            
         ICM   RE,1,JBPSLEN                                                     
         JZ    GAPL380                                                          
         AR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCODE(0),GAPTCODE                                          
         EX    RF,0(RE)                                                         
*        EXRL  RF,*-6                                                           
GAPL380  OC    SJ.JOBPCODE,SPACES                                               
         MVC   CSVKEY1,SJ.JOBPAS                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL390                                                          
         DC    H'0'                                                             
                                                                                
GAPL385  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL390                                                          
         DC    H'0'                                                             
                                                                                
GAPL390  CLC   CSVKEY1(JOBPCMED-JOBPASD),SJ.JOBPAS                              
         JNE   GAPL370             No other passives                            
         CLI   SJ.JOBPCMED,C' '                                                 
         JNH   GAPL385                                                          
         LA    RF,GAPPIN                                                        
         OC    GAPPIN,GAPPIN                                                    
         JNZ   *+8                                                              
         LA    RF,CCTPID                                                        
         CLI   GAPLCALT,GAPLBKAP   Back up approver search                      
         JNE   *+8                 No - normal approver                         
         LA    RF,GAPTBKUP                                                      
         CLC   SJ.JOBPPIDB,0(RF)   Exclude same pid                             
         JE    GAPL385                                                          
         XC    NEW.GAPTDAT1(GAPTLNQ),NEW.GAPTDAT1 Add new entry                 
         MVC   NEW.GAPTDAT1,GAPTDAT1 Add new entry                              
         MVC   NEW.GAPTCOFF,GAPTCOFF                                            
         MVC   NEW.GAPTACC,GAPTACC                                              
         MVI   NEW.GAPTSTA,GAPTSEQ                                              
         MVC   NEW.GAPTAPPL,GAPTAPPL                                            
         MVC   NEW.GAPTXXX,GAPTXXX                                              
         LLC   RE,GAPTLEN                                                       
         STC   RE,NEW.GAPTLEN                                                   
         LLC   RE,GAPTLVL                                                       
         AHI   RE,GAPTSL5                                                       
         STC   RE,NEW.GAPTLVL                                                   
         MVC   NEW.GAPTCMED(L'JOBPCMED),SJ.JOBPCMED                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,           +        
               NEW.GAPTDAT1,TSARABUF                                            
         JE    GAPL385                                                          
         TM    ATSRERRS,TSEDUP     We expect dupes as there can be many         
         JNZ   GAPL385             approvers with the same rights               
         DC    H'0'                                                             
         DROP  SJ,R2                                                            
                                                                                
*                                                                               
* 1R exceptions                                                                 
*                                                                               
GAPL400  CLI   GAPTLVL,GAPTSL5     Ignore records at person level               
         JE    GAPL490                                                          
                                                                                
DP       USING DPAPASD,IOKEY                                                    
         XC    DP.DPAPAS,DP.DPAPAS                                              
         MVI   DP.DPAPTYP,DPAPTYPQ                                              
         MVI   DP.DPAPSUB,DPAPSUBQ                                              
         MVC   DP.DPAPCPY,CUXCPY                                                
         MVC   DP.DPAPAPPL,GAPTPASS                                             
         ZAP   DP.DPAPXVAL,=P'0'                                                
         MVC   DP.DPAP1RAC,GAPTACT                                              
         MVC   CSVKEY1,DP.DPAPAS                                                
                                                                                
GAPL410  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GAPL422                                                          
         DC    H'0'                                                             
                                                                                
GAPL420  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    GAPL422                                                          
         DC    H'0'                                                             
                                                                                
GAPL422  CLC   CSVKEY1(DPAP1RAC-DPAPASD),DP.DPAPAS                              
         JNE   GAPL490             Go to next table entry                       
         CLI   GAPTDAT1,GAPTT9Q    Finance entry                                
         JNE   GAPL424             No                                           
         TM    DP.DPAPSTAT,DPAPDFLT  Yes - check for default as should          
         JZ    GAPL420             always have default approver                 
GAPL424  CLI   GAPTLVL,GAPTSL0                                                  
         JNE   GAPL426                                                          
         CLC   DP.DPAP1RAC,SPACES                                               
         JNH   GAPL490                                                          
         CLI   DP.DPAP1RAC,X'FF'                                                
         JE    GAPL490                                                          
         J     GAPL428                                                          
GAPL426  LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   DP.DPAP1RAC(0),GAPTACT                                           
         EX    RF,0(RE)                                                         
         JNE   GAPL490                                                          
                                                                                
GAPL428  LA    RF,GAPPIN                                                        
         OC    GAPPIN,GAPPIN                                                    
         JNZ   *+8                                                              
         LA    RF,CCTPID                                                        
         CLI   GAPLCALT,GAPLBKAP   Back up approver search                      
         JNE   *+8                 No - normal approver                         
         LA    RF,GAPTBKUP                                                      
         CLC   DP.DPAPPIDB,0(RF)   Exclude same pid                             
         JE    GAPL420                                                          
                                                                                
         CLI   DP.DPAP1RAC,C' '    Do we have an account                        
         JE    GAPL420             No                                           
         XC    NEW.GAPTDAT1(GAPTLNQ),NEW.GAPTDAT1                               
         LLC   RF,ONERL1L                                                       
         MVI   NEW.GAPTLVL,GAPTSL1                                              
         LA    RE,DP.DPAP1RAC(RF)                                               
         CLI   0(RE),C' '                                                       
         JE    GAPL430                                                          
         LLC   RF,ONERL2L                                                       
         MVI   NEW.GAPTLVL,GAPTSL2                                              
         LA    RE,DP.DPAP1RAC(RF)                                               
         CLI   0(RE),C' '                                                       
         JE    GAPL430                                                          
         LLC   RF,ONERL3L                                                       
         MVI   NEW.GAPTLVL,GAPTSL3                                              
         LA    RE,DP.DPAP1RAC(RF)                                               
         CLI   0(RE),C' '                                                       
         JE    GAPL430                                                          
         LLC   RF,ONERL4L                                                       
         MVI   NEW.GAPTLVL,GAPTSL4                                              
                                                                                
GAPL430  MVC   NEW.GAPTACT,DP.DPAP1RAC                                          
         MVC   NEW.GAPTDAT1,GAPTDAT1 Add new entry                              
         MVI   NEW.GAPTSTA,GAPTSEQ                                              
         MVC   NEW.GAPTAPPL,GAPTAPPL                                            
         MVC   NEW.GAPTXXX,GAPTXXX                                              
         STC   RF,NEW.GAPTLEN                                                   
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,           +        
               NEW.GAPTDAT1,TSARABUF                                            
         JE    GAPL420                                                          
         TM    ATSRERRS,TSEDUP     We expect dupes as there can be many         
         JNZ   GAPL420             approvers with the same rights               
         DC    H'0'                                                             
         DROP  DP                                                               
                                                                                
GAPL490  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         JE    GAPL220                                                          
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   *+6                 End of buffer                                
         DC    H'0'                                                             
                                                                                
GAPL600  J     EXITY                                                            
         DROP  RF,R3                                                            
*                                                                               
* SUPPRESS 'ALL ACCESS' ENTRY IF WE'VE FOUND AN ACCOUNT (LIMLIST)               
*                                                                               
SETRSTF  DS    0H                                                               
         CLI   GAPTDAT1,GAPTT2Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTAJOBS    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT6Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTAJOBS    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT5Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTAMED     CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT7Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTAETYP    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT3Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTA1NAC    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT1Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTASTAF    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT4Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS1,FF-RSTASCHM    CLEAR TO SUPPRESS 'ALL ACCESS'           
         CLI   GAPTDAT1,GAPTT8Q                                                 
         JNE   *+8                                                              
         NI    GLRSTAS2,FF-RSTASUPP    CLEAR TO SUPPRESS 'ALL ACCESS'           
         BR    RE                                                               
***********************************************************************         
* READ COMPANY, OFFICE LIST AND OFFICE RECORDS FOR CPXEL SETTINGS               
* GOVERNING ACCESS TO ACCOUNTS IF NO LIMLIST RECORDS EXIST FOR USER             
* BASED ON BRA11 ROUTINE                                                        
***********************************************************************         
ACCRUL   NTR1  LABEL=*,BASE=*                                                   
* presume all ok                                                                
         MVI   GLRSTAS1,RSTAJOBS+RSTAMED+RSTAETYP+RSTA1NAC+RSTASTAF+RST+        
               ASCHM+RSTAWC                                                     
         MVI   GLRSTAS2,RSTASUPP                                                
*                                                                               
MY       USING CPYELD,SCPYEL                                                    
         LA    RF,SCPXEL           COMPANY CPXEL                                
         GOTOR SETLAV              Set limit access values at company           
         OC    GLRSTAS1(2),GLRSTAS1                                             
         JZ    EXITY               COMPANY - NO ACCESS BY DEFAULT               
         CLI   CUACCS,C'$'         ACCESS VIA OFFICE LIST?                      
         JE    ACCRUL40            YES, READ OFFKEY FOR LIST REC                
*                                                                               
*  Look for office list level settings (2CO)                                    
*     assume we have an office code (indistinguisable in 2CO anyway)            
*     look for membership of office list, take settings if found                
*     NOTE: this will merge settings from ALL lists this office is              
*           set up on!                                                          
         USING OFLPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    OFLPAS,OFLPAS                                                    
         MVI   OFLPTYP,OFLPTYPQ                                                 
         MVI   OFLPSUB,OFLPSUBQ                                                 
         MVC   OFLPCPY,CUXCPY                                                   
         MVC   OFLPOFF,CUACCS+1                                                 
         TM    MY.CPYSTAT4,CPYSOFF2                                             
         JZ    *+10                                                             
         MVC   OFLPOFF,CUACCS+2    2CO IS $$NN                                  
         MVC   CSVKEY1,OFLPAS                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    ACCRUL20                                                         
         J     ACCRUL40                                                         
*                                                                               
ACCRUL10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   ACCRUL40                                                         
*                                                                               
ACCRUL20 CLC   OFLPAS(OFLPOFL-OFLPASD),CSVKEY1                                  
         JNE   ACCRUL40                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL30 CLI   N.CPXEL,0                                                        
         JE    ACCRUL10                                                         
         CLI   N.CPXEL,CPXELQ                                                   
         JE    ACCRUL35                                                         
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL30                                                         
         DROP  N                                                                
*                                                                               
ACCRUL35 DS    0H                                                               
         GOTOR SETLAV              SET LIMIT ACCESS VALUES AT OFL LVL           
         J     ACCRUL10                                                         
*                                                                               
* office level settings, or new (Flist) office list if 1CO                      
*     NOTE: Office list records use OFFKEY too, so only one read needed         
ACCRUL40 OC    GLRSTAS1(2),GLRSTAS1                                             
         JZ    EXITY               OLIST - NO ACCESS BY DEFAULT                 
*                                                                               
         OC    CUACCS,CUACCS       SINGLE-OFFICE ACCESS?                        
         JZ    EXITY               NO, UNRESTRICTED                             
         LA    R2,IOKEY            ELSE TRY OFFICE FOR SETTINGS                 
         USING OFFRECD,R2                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,CUACCS+1    SINGLE OFFICE, 1CO                           
         CLI   CUACCS,C'$'                                                      
         JNE   *+10                                                             
         MVC   OFFKOFF,CUACCS      OFFICE LIST,1CO -FLIST OFFICE LIST?          
         TM    MY.CPYSTAT4,CPYSOFF2                                             
         JZ    *+10                                                             
         MVC   OFFKOFF,CUACCS+2    2CO, OFFICE OR LIST                          
         MVC   CSVKEY1,OFFKEY                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   EXITY               NO OFFICE RECORD                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL50 CLI   N.CPXEL,0                                                        
         JE    EXITY                                                            
         CLI   N.CPXEL,CPXELQ                                                   
         JE    ACCRUL55                                                         
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL50                                                         
         DROP  N,MY                                                             
*                                                                               
ACCRUL55 DS    0H                                                               
         GOTOR SETLAV            SET LIMIT ACCESS VALUES AT OFF LVL             
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  UPDATE TRACKING FLAGS FROM THIS CPXEL (RF)                                   
N        USING CPXELD,RF                                                        
SETLAV   DS    0H                                                               
         TM    N.CPXSTAT3,CPXAMED                                               
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAMED     NO ACCESS BY DEFAULT                     
*                                                                               
         TM    N.CPXSTAT3,CPXAJOBS                                              
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAJOBS    NO ACCESS BY DEFAULT                     
*                                                                               
         TM    N.CPXSTAT3,CPXAWC                                                
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAWC      NO ACCESS BY DEFAULT                     
*                                                                               
         TM    N.CPXSTAT3,CPXAETYP                                              
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTAETYP     NO ACCESS BY DEFAULT                    
*                                                                               
         TM    N.CPXSTAT4,CPXASUPP                                              
         JZ    *+8                                                              
         NI    GLRSTAS2,FF-RSTASUPP     NO ACCESS BY DEFAULT                    
*                                                                               
         TM    N.CPXSTAT3,CPXASTAF                                              
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTASTAF     NO ACCESS BY DEFAULT                    
*                                                                               
         TM    N.CPXSTAT3,CPXA1NAC                                              
         JZ    *+8                                                              
         NI    GLRSTAS1,FF-RSTA1NAC     NO ACCESS BY DEFAULT                    
*                                                                               
         TM    N.CPXSTAT3,CPXASCHM                                              
         BZR   RE                                                               
         NI    GLRSTAS1,FF-RSTASCHM     NO ACCESS BY DEFAULT                    
         BR    RE                                                               
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
*  HANDLE OFFICES WHEN NOT ON LIDEL                                             
*     CLIENT-LEVEL LIDELS ONLY (INCLUDING WHERED SPOOFED TO PROD-LEVEL)         
*     IF SPOOFED (GAPTSMDQ), AND NO OFFICE KNOWN, WILL GET PRODUCT OFFC         
*                                                                               
* ENTRY: R5 IS A(SJ ACCOUNT                                                     
*        R2 IS A(APAPTBL ENTRY)                                                 
***********************************************************************         
GETSJOF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*GETSJOF'                                                    
         TM    SCPYEL+(CPYSTAT1-CPYELD),CPYSOROE  Is agency on offices          
         JZ    EXITN               No - not interested                          
         USING APAPTBLD,R2                                                      
         CLC   APAPLDG,PRODUL      Dealing with production ledger               
         JNE   EXITN               No - not interested                          
         TM    GAPTSTA,GAPTSMDQ    MOFIFIED LIDEL?                              
         JZ    GSJOF02                                                          
         LLC   RF,APAPLOFF         YES, MAY NEED TO SET OFFICE CODE             
         JZ    EXITN                                                            
         AR    RF,R6                                                            
         CLC   0(L'LIDLOFF,RF),SPACES                                           
         JNH   GSJOF03                                                          
         J     EXITN               ALREADY GIVEN, NO FURTHER ACTION             
GSJOF02  CLI   GAPLEVEL,GAPTSL1    Client level                                 
         JNE   EXITN               No - not interested                          
GSJOF03  TM    GAPWIND,GAPWOFF     Have we read for offices previously          
         JNZ   EXITY               Yes - nothing else to do                     
                                                                                
         MVC   GSVIOKEY,IOKEY                                                   
                                                                                
SJ       USING ACTRECD,IOKEY                                                    
         MVC   SJ.ACTKEY,SPACES    Read for client record                       
         MVC   SJ.ACTKCPY,CUXCPY                                                
         MVC   SJ.ACTKULA(2),PRODUL                                             
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   SJ.ACTKACT(0),0(R5)                                              
         EX    RE,0(RF)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    GSJOF06                                                          
         DC    H'0'                                                             
                                                                                
GSJOF06  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
                                                                                
         L     R2,AIO2                                                          
         AHI   R2,ACTRFST-ACTRECD  R2=A(elements)                               
                                                                                
         LA    R0,GAPOFFTB         Clear office table                           
         LA    R1,L'GAPOFFTB                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R3,GAPOFFTB                                                      
                                                                                
         USING PPRELD,R2                                                        
GSJOF08  CLI   PPREL,0                                                          
         JE    GSJOF16                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    GSJOF12                                                          
         CLI   PPREL,LIDELQ                                                     
         JE    GSJOF14                                                          
GSJOF10  LLC   R0,PPRLN                                                         
         AR    R2,R0                                                            
         J     GSJOF08                                                          
                                                                                
GSJOF12  MVC   0(L'LIDLOFF,R3),PPRGAOFF                                         
         OC    0(L'LIDLOFF,R3),SPACES                                           
         LA    R3,L'LIDLOFF(R3)                                                 
         J     GSJOF10                                                          
                                                                                
         USING LIDELD,R2                                                        
GSJOF14  CLI   LIDTYPE,LIDTPOFC                                                 
         JNE   GSJOF10                                                          
         LLC   RF,LIDLN                                                         
         SHI   RF,LIDLNDQ+1                                                     
         BASR  R1,0                                                             
         MVC   0(0,R3),LIDDATA                                                  
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         J     GSJOF10                                                          
                                                                                
GSJOF16  MVC   IOKEY,GSVIOKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GSJOF18                                                          
         DC    H'0'                                                             
                                                                                
GSJOF18  OC    GAPOFFTB+L'LIDLOFF(L'LIDLOFF),GAPOFFTB+L'LIDLOFF                 
         JZ    EXITN               No                                           
         OI    GAPWIND,GAPWOFF     Set office table in use                      
         J     EXITY                                                            
         EJECT                                                                  
         DROP  SJ,NEW,R2,R4,RC                                                  
                                                                                
GLOBALS  DS    0D                                                               
         LTORG                                                                  
JBPSTBL  DS    0X                                                               
         DC    AL2(PCLILEN-WORKD)                                               
         DC    AL1(GAPTACC-GAPTABD,L'GAPTCOFF)                                  
         DC    AL2(0)                                                           
         DC    AL1(GAPTACC-GAPTABD,L'GAPTCOFF)                                  
         DC    AL2(0)                                                           
         DC    AL1(GAPTCOFF-GAPTABD,0)                                          
         DC    X'FF'                                                            
                                                                                
GPRTTAB  DS    0X                                                               
         DC    AL1(GAPTT1Q,GAPLAPPR,LIDTAP1R),CL2'1R'                           
         DC    AL1(GAPTT1Q,GAPLBKAP,LIDTAP1R),CL2'1R'                           
         DC    AL1(GAPTT1Q,GAPLLIML,LIDT1RAC),CL2'1R'                           
         DC    AL1(GAPTT2Q,GAPLAPPR,LIDTAPSJ),CL2'SJ'                           
         DC    AL1(GAPTT2Q,GAPLBKAP,LIDTAPSJ),CL2'SJ'                           
         DC    AL1(GAPTT2Q,GAPLLIML,LIDTCPJL),CL2'SJ'                           
         DC    AL1(GAPTT3Q,GAPLAPPR,LIDTAP1N),CL2'1N'                           
         DC    AL1(GAPTT3Q,GAPLBKAP,LIDTAP1N),CL2'1N'                           
         DC    AL1(GAPTT3Q,GAPLLIML,LIDTNCLL),CL2'1N'                           
         DC    AL1(GAPTT4Q,GAPLLIML,LIDTESCH),CL2'XX'                           
         DC    AL1(GAPTT5Q,GAPLLIML,LIDTMEDL),CL2'XX'                           
         DC    AL1(GAPTT7Q,GAPLLIML,LIDTEXPL),CL2'XX'                           
         DC    AL1(GAPTT8Q,GAPLLIML,LIDTSUPP),CL2'SX'                           
         DC    AL1(GAPTT9Q,GAPLAPPR,LIDTAP1R),CL2'1R'                           
         DC    AL1(GAPTT9Q,GAPLBKAP,LIDTAP1R),CL2'1R'                           
         DC    AL1(GAPTTAQ,GAPLLIML,LIDTWCL),CL2'SJ'                            
         DC    AL1(GAPTT20Q,GAPLAPPR,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT20Q,GAPLBKAP,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT20Q,GAPLLIML,LIDTCPJL),CL2'SJ'                          
         DC    AL1(GAPTT20Q,GAPLAPPR,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT20Q,GAPLBKAP,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT20Q,GAPLLIML,LIDTNCLL),CL2'1N'                          
         DC    AL1(GAPTT21Q,GAPLLIML,LIDTCPJL),CL2'SJ'                          
         DC    AL1(GAPTT21Q,GAPLLIML,LIDTESCH),CL2'XX'                          
         DC    AL1(GAPTT21Q,GAPLLIML,LIDTMEDL),CL2'XX'                          
         DC    AL1(GAPTT22Q,GAPLAPPR,LIDTAP1R),CL2'1R'                          
         DC    AL1(GAPTT22Q,GAPLBKAP,LIDTAP1R),CL2'1R'                          
         DC    AL1(GAPTT22Q,GAPLLIML,LIDT1RAC),CL2'1R'                          
         DC    AL1(GAPTT22Q,GAPLAPPR,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT22Q,GAPLBKAP,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT22Q,GAPLLIML,LIDTCPJL),CL2'SJ'                          
         DC    AL1(GAPTT22Q,GAPLAPPR,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT22Q,GAPLBKAP,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT22Q,GAPLLIML,LIDTNCLL),CL2'1N'                          
         DC    AL1(GAPTT23Q,GAPLLIML,LIDTCPJL),CL2'SJ'                          
         DC    AL1(GAPTT23Q,GAPLLIML,LIDTSUPP),CL2'SX'                          
         DC    AL1(GAPTT23Q,GAPLLIML,LIDTEXPL),CL2'XX'                          
         DC    AL1(GAPTT23Q,GAPLLIML,LIDTMEDL),CL2'XX'                          
         DC    AL1(GAPTT24Q,GAPLAPPR,LIDTAP1R),CL2'1R'                          
         DC    AL1(GAPTT24Q,GAPLBKAP,LIDTAP1R),CL2'1R'                          
         DC    AL1(GAPTT24Q,GAPLLIML,LIDT1RAC),CL2'1R'                          
         DC    AL1(GAPTT24Q,GAPLAPPR,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT24Q,GAPLBKAP,LIDTAPSJ),CL2'SJ'                          
         DC    AL1(GAPTT24Q,GAPLLIML,LIDTCPJL),CL2'SJ'                          
         DC    AL1(GAPTT24Q,GAPLAPPR,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT24Q,GAPLBKAP,LIDTAP1N),CL2'1N'                          
         DC    AL1(GAPTT24Q,GAPLLIML,LIDTNCLL),CL2'1N'                          
         DC    AL1(GAPTT24Q,GAPLLIML,LIDTWCL),CL2'SJ'                           
         DC    X'FF'                                                            
                                                                                
APAPTBL  DS    0X                                                               
SJAPTBL  DS    0X                                                               
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QTIME,LIDTAPSJ,0,BHQ,LIDATIME,0)                             
         DC    AL1(TSJPSJAQ,TAPPCLI,GAPTT2Q,JOBPATIM)                           
         DC    AL1(0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                      
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA)                                            
         DC    AL1(QTIME,LIDTAPSJ,0,BNHQ,LIDATIME,0)                            
         DC    AL1(TSJPMEDQ,TAPPMED,GAPTT6Q,JOBPATIM)                           
         DC    AL1(GAPTMED-GAPTABD)                                             
         DC    AL1(GAPTOFF-GAPTABD,GAPTCPJ-GAPTABD,0,L'GAPTMED)                 
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QTIME,LIDTCPJL,0,0,LIDLTIME,0)                               
         DC    AL1(TSJPSJAQ,0,GAPTT2Q,0)                                        
         DC    AL1(0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                      
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QEXPCLM,LIDTAPSJ,BNZQ,BHQ,LIDAEXPN,0)                        
         DC    AL1(EXJPCLI1,EXSPCLI1,GAPTT2Q,JOBPAEXP)                          
         DC    AL1(0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                      
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA)                                            
         DC    AL1(QEXPCLM,LIDTAPSJ,BNZQ,BNHQ,LIDAEXPN,0)                       
         DC    AL1(EXJPMED1,EXSPMED1,GAPTT6Q,JOBPAEXP)                          
         DC    AL1(GAPTMED-GAPTABD,GAPTOFF-GAPTABD)                             
         DC    AL1(GAPTCPJ-GAPTABD,0,L'GAPTMED)                                 
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA)                                            
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BNHQ,0,LIDAEL1B)                        
         DC    AL1(EXJPMBL1,EXSPMBL1,GAPTT6Q,JOBPAX1B)                          
         DC    AL1(GAPTMED-GAPTABD,GAPTOFF-GAPTABD)                             
         DC    AL1(GAPTCPJ-GAPTABD,0,L'GAPTMED)                                 
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BHQ,0,LIDAEL1B)                         
         DC    AL1(EXJPCBL1,EXSPCBL1,GAPTT2Q,JOBPAX1B)                          
         DC    AL1(0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                      
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA)                                            
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BNHQ,0,LIDAEL1N)                        
         DC    AL1(EXJPMNB1,EXSPMNB1,GAPTT6Q,JOBPAX1N)                          
         DC    AL1(GAPTMED-GAPTABD,GAPTOFF-GAPTABD)                             
         DC    AL1(GAPTCPJ-GAPTABD,0,L'GAPTMED)                                 
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BHQ,0,LIDAEL1N)                         
         DC    AL1(EXJPCNB1,EXSPCNB1,GAPTT2Q,JOBPAX1N)                          
         DC    AL1(0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                      
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA)                                            
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BNHQ,0,LIDAEL2B)                        
         DC    AL1(EXJPMBL2,EXSPMBL2,GAPTT6Q,JOBPAX2B)                          
         DC    AL1(GAPTMED-GAPTABD,GAPTOFF-GAPTABD,GAPTCPJ-GAPTABD,0)           
         DC    AL1(L'GAPTMED)                                                   
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BHQ,0,LIDAEL2B)                         
         DC    AL1(EXJPCBL2,EXSPCBL2,GAPTT2Q,JOBPAX2B,0)                        
         DC    AL1(GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                        
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,LIDASJME-LIDDATA)                       
         DC    AL1(LIDASJOF-LIDDATA,QEXPCLM,LIDTAPSJ,BZQ,BNHQ,0)                
         DC    AL1(LIDAEL2N,EXJPMNB2,EXSPMNB2,GAPTT6Q,JOBPAX2N)                 
         DC    AL1(GAPTMED-GAPTABD,GAPTOFF-GAPTABD,GAPTCPJ-GAPTABD,0)           
         DC    AL1(L'GAPTMED)                                                   
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDASJAC-LIDDATA,0,LIDASJOF-LIDDATA)                     
         DC    AL1(QEXPCLM,LIDTAPSJ,BZQ,BHQ,0,LIDAEL2N)                         
         DC    AL1(EXJPCNB2,EXSPCNB2,GAPTT2Q,JOBPAX2N,0)                        
         DC    AL1(GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                        
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEXPCLM,LIDTCPJL,BNZQ,0,LIDLEXPN,0)                          
         DC    AL1(EXJPCLI1,0,GAPTT2Q,0,0)                                      
         DC    AL1(GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)                        
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEXPCLM,LIDTCPJL,BZQ,0,LIDLEXPN,0,EXJPCBL1,0)                
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEXPCLM,LIDTCPJL,BZQ,0,LIDLEXPN,0,EXJPCNB1,0)                
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEXPCLM,LIDTCPJL,BZQ,0,LIDLEXPN,0,EXJPCBL2,0)                
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEXPCLM,LIDTCPJL,BZQ,0,LIDLEXPN,0,EXJPCNB2,0)                
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QEST,LIDTCPJL,0,0,LIDLESTM,0,0,0)                            
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QRESRCES,LIDTCPJL,0,0,LIDLRESC,0,0,0)                        
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QINV,LIDTCPJL,0,0,LIDLINVC,0,0,0)                            
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QJOB,LIDTCPJL,0,0,LIDLJOBS,0,0,0)                            
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
         DC    CL2'SJ'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,LIDLOFF-LIDDATA)                       
         DC    AL1(QORD,LIDTCPJL,0,0,LIDLORDS,0,0,0)                            
         DC    AL1(GAPTT2Q,0,0,GAPTCOFF-GAPTABD,GAPTACC-GAPTABD,0,0)            
ORAPTBL  DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QTIME,LIDT1RAC,0,0,LIDLTIME,0)                               
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QTIME,LIDTAP1R,0,0,LIDAPDTI,0,0)                             
         DC    AL1(TAPPMAN,GAPTT1Q,DPAPATIM,0,0,GAPTACT-GAPTABD,0,0)            
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QTIMOFF,LIDTAP1R,0,0,0,LIDAPDTO,0)                           
         DC    AL1(0,GAPTT1Q,DPAPATIO,0,0,GAPTACT-GAPTABD,0,0)                  
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QEXPCLM,LIDT1RAC,BNZQ,0,LIDLEXPN,0,0)                        
         DC    AL1(0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                         
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,BNZQ,0,LIDAPDEX,0,0)                        
         DC    AL1(EXSPLMN1,GAPTT1Q,DPAPAEXP,0,0,GAPTACT-GAPTABD,0,0)           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDLACT-LIDDATA,0,0)                              
         DC    AL1(QTIME,LIDT1RAC,0,0,LIDLTIME,0,0)                             
         DC    AL1(0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                         
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QTIME,LIDTAP1R,0,0,LIDAPDTI,0,0)                             
         DC    AL1(TAPPMAN,GAPTT1Q,DPAPATIM,0,0,GAPTACT-GAPTABD,0,0)            
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDLACT-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDT1RAC,BNZQ,0,LIDLEXPN,0,0)                        
         DC    AL1(0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                         
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QEXPCLM,LIDTAP1R,BNZQ,0,LIDAPDEX,0,0)                        
         DC    AL1(EXSPLMN1,GAPTT1Q,DPAPAEXP,0,0,GAPTACT-GAPTABD,0,0)           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT9Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,0,0,LIDAPDEF,0,0)                           
         DC    AL1(EXSPLFN1,GAPTT9Q,DPAPAEXF,0,0,GAPTACT-GAPTABD,0,0)           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT9Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,0,0,LIDAPDED,0,0)                           
         DC    AL1(EXSPLFN1,GAPTT9Q,DPAPAEXF,0,0,GAPTACT-GAPTABD,0,0)           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QEXPCLM,LIDT1RAC,BZQ,0,LIDLEXPN,0,EXDPNBL1,0)                
         DC    AL1(GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QEXPCLM,LIDT1RAC,BZQ,0,LIDLEXPN,0,EXDPNBL2,0)                
         DC    AL1(GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDE1,0,EXDPNBL1,EXSPRNB1)         
         DC    AL1(GAPTT1Q,DPAPAEX1,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDE2,0,EXDPNBL2,EXSPRNB2)         
         DC    AL1(GAPTT1Q,DPAPAEX2,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDB1,0,EXDPBIL1,EXSPRBL1)         
         DC    AL1(GAPTT1Q,DPAPAEB1,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT1Q,0,LIDAPACC-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDB2,0,EXDPBIL2,EXSPRBL2)         
         DC    AL1(GAPTT1Q,DPAPAEB2,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                   ADDED FOR LIST/SEARCH CALL             
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QEST,LIDT1RAC,0,0,LIDLESTM,0)                                
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                   ADDED FOR LIST/SEARCH CALL             
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QINV,LIDT1RAC,0,0,LIDLINVC,0)                                
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                   ADDED FOR LIST/SEARCH CALL             
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QJOB,LIDT1RAC,0,0,LIDLJOBS,0)                                
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                   ADDED FOR LIST/SEARCH CALL             
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QORD,LIDT1RAC,0,0,LIDLORDS,0)                                
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                   ADDED FOR LIST/SEARCH CALL             
         DC    AL1(GAPTT1Q,0,LIDLACT-LIDDATA,0,0)                               
         DC    AL1(QRESRCES,LIDT1RAC,0,0,LIDLRESC,0)                            
         DC    AL1(0,0,GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                       
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDLACT-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDT1RAC,BZQ,0,LIDLEXPN,0,EXDPNBL1,0)                
         DC    AL1(GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDLACT-LIDDATA,0,0)                              
         DC    AL1(QEXPCLM,LIDT1RAC,BZQ,0,LIDLEXPN,0,EXDPNBL2,0)                
         DC    AL1(GAPTT1Q,0,0,0,GAPTACT-GAPTABD,0,0)                           
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDE1,0,EXDPNBL1,EXSPRNB1)         
         DC    AL1(GAPTT1Q,DPAPAEX1,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDE2,0,EXDPNBL2,EXSPRNB2)         
         DC    AL1(GAPTT1Q,DPAPAEX2,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDB1,0,EXDPBIL1,EXSPRBL1)         
         DC    AL1(GAPTT1Q,DPAPAEB1,0,0,GAPTACT-GAPTABD,0,0)                    
         DC    CL2'1R'                                                          
         DC    AL1(GAPTT22Q,0,LIDAPACC-LIDDATA,0,0)                             
         DC    AL1(QEXPCLM,LIDTAP1R,BZQ,0,LIDAPDB2,0,EXDPBIL2,EXSPRBL2)         
         DC    AL1(GAPTT1Q,DPAPAEB2,0,0,GAPTACT-GAPTABD,0,0)                    
ONAPTBL  DC    CL2'1N'                                                          
         DC    AL1(0,0,LIDLACT-LIDDATA,0,0)                                     
         DC    AL1(QTIME,LIDTNCLL,0,0,LIDLTIME,0)                               
         DC    AL1(TSJP1NAQ,0,GAPTT3Q,0,0,0)                                    
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'                                                          
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QTIME,LIDTAP1N,0,0,LIDATIME,0)                               
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QEST,LIDTAP1N,0,0,LIDLESTM,0)                                
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QEXPCLM,LIDTAP1N,0,0,LIDLEXPN,0)                             
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QINV,LIDTAP1N,0,0,LIDLINVC,0)                                
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QJOB,LIDTAP1N,0,0,LIDLJOBS,0)                                
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QORD,LIDTAP1N,0,0,LIDLORDS,0)                                
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
         DC    CL2'1N'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,LIDA1NAC-LIDDATA,0,0)                                    
         DC    AL1(QRESRCES,LIDTAP1N,0,0,LIDLRESC,0)                            
         DC    AL1(TSJP1NAQ,TAPP1NA,GAPTT3Q,0,0,0)                              
         DC    AL1(GAPTACT-GAPTABD,0,0)                                         
CRAPTBL  DC    CL2'SX'                                                          
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QINV,LIDTSUPP,0,0,LIDLINVC,0)                                
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QEST,LIDTSUPP,0,0,LIDLESTM,0)                                
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QEXPCLM,LIDTSUPP,0,0,LIDLEXPN,0)                             
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QJOB,LIDTSUPP,0,0,LIDLJOBS,0)                                
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QORD,LIDTSUPP,0,0,LIDLORDS,0)                                
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QRESRCES,LIDTSUPP,0,0,LIDLRESC,0)                            
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
         DC    CL2'SX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,LIDLSULA-LIDDATA,LIDLSACT-LIDDATA)                         
         DC    AL1(0,LIDLOFFC-LIDDATA)                                          
         DC    AL1(QTIME,LIDTSUPP,0,0,LIDLTIME,0)                               
         DC    AL1(0,0,GAPTT8Q,0,0,GAPTCOFC-GAPTABD)                            
         DC    AL1(GAPTCACT-GAPTABD,GAPTCRED-GAPTABD,0)                         
ESAPTBL  DC    CL2'XX'                                                          
         DC    AL1(0,0,0,LIDLSCH-LIDDATA,0)                                     
         DC    AL1(QEST,LIDTESCH,0,0,LIDLESTM,0,0,0)                            
         DC    AL1(GAPTT4Q,0,GAPTSCH-GAPTABD,0,0,0,L'GAPTSCH)                   
ETAPTBL  DC    CL2'XX'                                                          
         DC    AL1(0,0,0,LIDLETY-LIDDATA,0)                                     
         DC    AL1(QINV,LIDTEXPL,0,0,LIDLINVC,0,0,0)                            
         DC    AL1(GAPTT7Q,0,GAPTETYP-GAPTABD,0,0,0,L'GAPTETYP)                 
         DC    CL2'XX'                                                          
         DC    AL1(0,0,0,LIDLETY-LIDDATA,0)                                     
         DC    AL1(QORD,LIDTEXPL,0,0,LIDLORDS,0,0,0)                            
         DC    AL1(GAPTT7Q,0,GAPTETYP-GAPTABD,0,0,0,L'GAPTETYP)                 
MEAPTBL  DC    CL2'XX'                                                          
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QEST,LIDTMEDL,0,0,LIDLESTM,0,0,0)                            
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QEXPCLM,LIDTMEDL,0,0,LIDLEXPN,0,0,0)                         
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QINV,LIDTMEDL,0,0,LIDLINVC,0,0,0)                            
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QJOB,LIDTMEDL,0,0,LIDLJOBS,0,0,0)                            
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QORD,LIDTMEDL,0,0,LIDLORDS,0,0,0)                            
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QRESRCES,LIDTMEDL,0,0,LIDLRESC,0,0,0)                        
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    CL2'XX'             ADDED FOR LIST/SEARCH CALL                   
         DC    AL1(0,0,0,LIDLMED-LIDDATA,0)                                     
         DC    AL1(QTIME,LIDTMEDL,0,0,LIDLTIME,0,0,0)                           
         DC    AL1(GAPTT5Q,0,GAPTMEDI-GAPTABD,0,0,0,L'GAPTMEDI)                 
         DC    X'FF'                                                            
*                                                                               
*  GAPLST REQUEST/RSTEL FLAG TRANSLATION FOR 'ALL ACCESS' ENTRIES               
RQRSTTAB DC    0XL3                                                             
         DC    AL1(GAPTT1Q),AL1(RSTASTAF,0)                                     
         DC    AL1(GAPTT2Q),AL1(RSTAJOBS,0)                                     
         DC    AL1(GAPTT3Q),AL1(RSTA1NAC,0)                                     
         DC    AL1(GAPTT4Q),AL1(RSTASCHM,0)                                     
         DC    AL1(GAPTT5Q),AL1(RSTAMED,0)                                      
         DC    AL1(GAPTT6Q),AL1(RSTAJOBS,0)                                     
         DC    AL1(GAPTT7Q),AL1(RSTAETYP,0)                                     
         DC    AL1(GAPTT8Q),AL1(0,RSTASUPP)                                     
         DC    AL1(GAPTT9Q),AL1(RSTASTAF,0)                                     
         DC    AL1(GAPTTAQ),AL1(RSTAWC,0)                                       
         DC    AL1(GAPTT20Q),AL1(RSTAJOBS+RSTA1NAC,0)                           
         DC    AL1(GAPTT21Q),AL1(RSTAJOBS+RSTAMED+RSTASCHM,0)                   
         DC    AL1(GAPTT22Q),AL1(RSTASTAF+RSTAJOBS+RSTA1NAC,0)                  
         DC    AL1(GAPTT23Q),AL1(RSTAJOBS+RSTAMED+RSTAETYP,RSTASUPP)            
         DC    AL1(GAPTT24Q),AL1(RSTASTAF+RSTAJOBS+RSTA1NAC+RSTAWC,0)           
         DC    AL1(0)           EOT                                             
*                                                                               
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
                                                                                
GAPWRKD  DSECT                     ** GAPLST S/R LOCAL W/S **                   
GENDELE  DS    F                   Address of where element ends                
GAPLCALT DS    XL1                 Calling type                                 
*                                  See GAPTABD for definitions                  
GAPLDAT1 DS    XL1                 Data required                                
GAPLTYPE DS    XL1                 Type of view                                 
*                                  required by calling program                  
GAPLPARM DS    XL1                 Parameter byte to control behaviour          
*                                                                               
GAPLMODL DS    XL1                 calling module (neq calling server,          
*                                         eg account search for time)           
GAPPIN   DS    XL2                 Passed pin of person                         
GLRSTAS1 DS    XL1                 surviving rstacst1/2 flags trigger           
GLRSTAS2 DS    XL1                 'all access' entries in limlt calls          
*                                                                               
GAPLFILT DS    0CL14               Filter value                                 
GAPLRUNL DS    CL2                                                              
GAPLRACT DS    CL12                                                             
*                                                                               
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
GSEQUNCE DS    H                   Counter                                      
GSVLDG   DS    CL(L'ACTKUNT+L'ACTKLDG) Ledger last read from table              
GSVLDGRD DS    CL(L'ACTKUNT+L'ACTKLDG) Ledger last read for filter              
GSVIOKEY DS    XL(L'IOKEY)         Save IOKEY                                   
GAPFLTLN DS    XL1                 Filter length                                
GAPACTLN DS    XL1                 Account length                               
GAPLEVEL DS    XL1                 Level for GAPTAB                             
ATSRERRS DS    XL1                 Tsar buffer error                            
GAPLBPID DS    XL(L'PIDKPIDO)      Real approver pid from back up               
GAPWIND  DS    XL1                 Indicator                                    
GAPWOFF  EQU   X'80'               Ofices found                                 
GAPOFFTB DS    XL(ONEK)            Table of office codes                        
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPAREA2 DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPLTSAR DS    XL(GAPTLNQ)         Saved high-water mark                        
GAPWRKL  EQU   *-GAPWRKD                                                        
                                                                                
GPRTTABD DSECT                                                                  
GPRTDAT  DS    XL1                 Data type                                    
GPRTTYP  DS    XL1                 Type of call back up, approver               
GPRTLID  DS    XL1                 LIDELD type of data                          
GPRTLDG  DS    CL2                 Unit/ledger                                  
GPRTTABL EQU   *-GPRTTABD                                                       
                                                                                
JBPSTBLD DSECT                                                                  
JBPSDIS  DS    XL2                 Displacement to length                       
JBPSPOS  DS    XL1                 Position in GAPTABD                          
JBPSLEN  DS    XL1                 Fixed length to add                          
JBPSTBLL EQU   *-JBPSTBLD                                                       
                                                                                
APAPTBLD DSECT                     Approver application table                   
APAPLDG  DS    CL2                 Ledger to filter against                     
APAPDTYP DS    XL1                 GAPLDAT1 type                                
APAPLUNL DS    XL1                 LIDELD displacement to unit/ledger           
APAPLACT DS    XL1                 LIDELD displacement to account code          
APAPLMED DS    XL1                 LIDELD disp of media or other data           
APAPLOFF DS    XL1                 LIDELD displacement of office code           
APAPMOD  DS    XL1                 Calling module                               
APAPLTYP DS    XL1                 LIDELD type                                  
APAPWPP  DS    XL1                 condition code for WPP workflow              
APAPMED  DS    XL1                 condition code for media code                
APAPLST1 DS    XL1                 LIDELD status byte 1                         
APAPLST2 DS    XL1                 LIDELD status byte 2                         
APAPDTE  DS    XL1                 Application passive type for date            
APAPSTA  DS    XL1                 Application passive type for status          
APAPGTYP DS    XL1                 GAPTAB type                                  
APAPPTYP DS    XL1                 JOBPASD/DPAPASD application type             
APAPGMED DS    XL1                 GAPTAB pos of media or other code            
APAPGOFF DS    XL1                 GAPTAB position of office code               
APAPGACT DS    XL1                 GAPTAB position of account code              
APAPGUNL DS    XL1                 GAPTAB position of unit ledger               
APAPGLEN DS    XL1                 GAPTAB length of other data                  
APAPTBLQ EQU   *-APAPTBLD                                                       
                                                                                
ACBRA02  CSECT                                                                  
***********************************************************************         
* TSAR INTERFACE ROUTINE - TSAR USES AIO1                             *         
*                                                                     *         
* NTRY - P1 = TSAR ACTION (DEFINED IN DDTSARD)                        *         
*        P2 = A(TSAR BUFFER)                                          *         
*        P3 = TYPE OF BUFFER                                          *         
*                                                                     *         
* EXIT - CC=EQU - ACTION COMPLETED WITHOUT ERRORS                     *         
*        CC=NEQ - TSARERRS=TSAR ERROR CODE                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
MTSAR    J     *+12                                                             
         DC    CL8'*MTSAR*'                                                     
         LR    RB,RF                                                            
         USING MTSAR,RB                                                         
                                                                                
         L     R3,4(R1)            P2=TSAR BUFFER                               
         USING TSARD,R3                                                         
         MVC   TSACTN,3(R1)        P1=TSAR ACTION                               
         MVC   TSARTYP,11(R1)      P3=TSAR TYPE                                 
*                                                                               
         CLI   TSACTN,TSASRT                                                    
         BNE   MTSAR02                                                          
         L     R1,12(R1)                                                        
         MVC   TSRTPARM,0(R1)                                                   
*                                                                               
MTSAR02  CLI   TSACTN,TSAINI       *** TSAR INITIALIZE ***                      
         BNE   MTSAR12                                                          
         XC    0(TSPNEWL,R3),0(R3) CLEAR BLOCK                                  
         L     R1,AIO1                                                          
         ST    R1,TSAREC           ADDRESS OF RECORD                            
         MVI   TSACTN,TSAINI       INIT TSAR - CLEAR SPACE                      
         MVC   TSACOM,ACOMFACS                                                  
*                                                                               
MTSAR04  CLI   TSARTYP,TSAREMRG    ESTIMATE MERGE?                              
         BE    MTSAR06                                                          
         CLI   TSARTYP,TSARESBS    ESTIMATE SIDE BY SIDE?                       
         BNE   MTSAR08                                                          
MTSAR06  LHI   R0,ONEK                                                          
         OC    TSBUFFL,TSBUFFL                                                  
         JNZ   *+8                                                              
         STCM  R0,3,TSBUFFL        Set require 2MB off-line                     
         MVI   TSRECI,TSRXTN+TSRVAR+TSRTSAB1                                    
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         MVI   TSIND2,TSI2MANY                                                  
         MVI   TSKEYL,24           KEY LENGTH                                   
         LHI   R1,300                                                           
         STH   R1,TSRECL           MAX RECORD LENGTH                            
         B     MTSAR14                                                          
*                                                                               
MTSAR08  CLI   TSARTYP,TSARTDSR                                                 
         BE    MTSAR10                                                          
         CLI   TSARTYP,TSARTDL                                                  
         BE    MTSAR10                                                          
         DC    H'0'                                                             
MTSAR10  LH    R0,=AL2(100)                                                     
         OC    TSBUFFL,TSBUFFL                                                  
         JNZ   *+8                                                              
         STCM  R0,3,TSBUFFL        Set require 2MB off-line                     
         MVI   TSRECI,TSRXTN+TSRWSSVR+TSRMGB                                    
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         MVI   TSIND2,TSI2MANY                                                  
         MVI   TSKEYL,TD#KYLNQ    KEY LENGTH                                    
         LHI   R1,TD#LENQ                                                       
         STH   R1,TSRECL          MAX RECORD LENGTH                             
*                                                                               
MTSAR12  CLI   TSACTN,TSARDH      *** TSAR READ HIGH ***                        
         BNE   *+10                                                             
         XC    TSRNUM,TSRNUM                                                    
*                                                                               
MTSAR14  GOTO1 VTSAR,TSARD                                                      
         MVC   TSARERRS,TSERRS     PASS BACK ERRORS                             
         CLI   TSERRS,0                                                         
         JNE   EXITH               EXIT WITH ERROR                              
*                                                                               
         J     EXITY                                                            
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET COST PROFILES                                                   *         
* ON NTRY PARM 1 = 1R ACCOUNT CODE                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CSTPRF   J     *+12                                                             
         DC    CL8'*CSTPRF*'                                                    
         LR    RB,RF                                                            
         USING CSTPRF,RB                                                        
         L     R3,0(R1)                                                         
         L     R0,ACOBLOCK                                                      
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,ACOBLOCK                                                      
         USING COBLOCKD,R2                                                      
         MVC   COADM,VDATAMGR      PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CUXCPY                                                    
         MVC   COKMTHD,SPACES                                                   
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   COKOFC(0),0(R3)                                                  
         EX    RE,*-6                                                           
         OC    COKOFC,SPACES                                                    
         AHI   RE,1                                                             
         AR    R3,RE                                                            
*&&UK                                                                           
         MVC   COKDPT(2),0(R3)                                                  
         MVC   COKSDT(2),2(R3)                                                  
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         IC    RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   COKPER(0),4(R3)                                                  
         EX    RF,*-6                                                           
*&&                                                                             
*&&US                                                                           
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   COKDPT(0),0(R3)                                                  
         LA    R3,1(R1,R3)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   COKSDT(0),0(R3)                                                  
         LA    R3,1(R1,R3)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   COKPER(0),0(R3)                                                  
*&&                                                                             
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         DROP  RB                                                               
***********************************************************************         
* CLEAR IO AREA                                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CLRIO    J     *+12                                                             
         DC    CL8'*CLRIO**'                                                    
         LR    RB,RF                                                            
         USING CLRIO,RB                                                         
         L     RE,0(R1)            A(AIO AREA)                                  
         LTR   R1,R1                                                            
         JZ    EXITN                                                            
         LA    RF,2000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Interface to approval TSAR                                          *         
* P1(1)   = ACTION                                                              
* P1(2-4) = A(RETURN ERROR BYTE)                                      *         
* P2(1)   =                                                           *         
* P2(2-4) = A(SORT CARD) (ONLY IF TSASRT)                             *         
* P3(1)   =                                            *                        
* P3(2-4) = A(TSAR IO AREA)                                           *         
* P4(1)   =                                                           *         
* P4(2-4) = A(TSAR BLOCK)                                            *          
*** USES MINIO 2 BUFFER ***                                           *         
***********************************************************************         
                                                                                
GOATSR   J     *+12                                                             
         DC    C'*GOATSR*'                                                      
                                                                                
         LR    RB,RF                                                            
         USING GOATSR,RB                                                        
         USING GOAWRKD,RC                                                       
         LR    R2,R1               R2=A(Caller's parameter list)                
         L     R4,12(R2)           R1=A(TSAR block)                             
         MVC   GOTSABUF,0(R1)      * DOESN'T DO ANYTHING?                       
         L     R3,0(R2)            R3=A(error byte)                             
AB       USING TSARD,R4                                                         
         L     R1,8(R2)                                                         
         ST    R1,AB.TSAREC        Address of record buffer area                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOATSR02                                                         
         XC    AB.TSARD(TSPNEWL),AB.TSARD                                       
         MVC   AB.TSACTN,0(R2)                                                  
         MVC   AB.TSACOM,ACOMFACS                                               
         LHI   R0,ONEK                                                          
         OC    AB.TSBUFFL,AB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,AB.TSBUFFL      Set require 1MB off-line                    
         MVI   AB.TSRECI,TSRXTN+TSRMINB2                                        
         MVI   AB.TSKEYL,GAPTKLNQ   Set key length                              
         LHI   R0,GAPTLNQ           Set record length                           
         STCM  R0,3,AB.TSRECL                                                   
         MVI   AB.TSINDS,TSINODSK   Set no disk writes (save/restore)           
         MVI   AB.TSIND2,TSI2MANY                                               
         GOTOR VTSAR,AB.TSARD                                                   
         TM    AB.TSINDS,TSIINIOK                                               
         JNZ   EXITY                                                            
         DC    H'0'                 Initialisation failure                      
                                                                                
GOATSR02 TM    AB.TSINDS,TSIINIOK   Test initialised                            
         JZ    GOATSR06                                                         
                                                                                
         MVC   AB.TSACTN,0(R2)      Set action                                  
                                                                                
         CLI   AB.TSACTN,TSASRT     Test sorting                                
         JNE   GOATSR04                                                         
         L     R1,4(R2)                                                         
         MVC   AB.TSRTPARM,0(R1)    Yes - set sort parameters                   
                                                                                
GOATSR04 GOTOR VTSAR,AB.TSARD       Call TSAR                                   
         MVC   0(L'TSERRS,R3),AB.TSERRS   Return TSARERRS                       
         J     GOATSRX                                                          
                                                                                
GOATSR06 MVI   0(R3),TSEEOF                                                     
                                                                                
GOATSRX  CLI   0(R3),0     Set condition code for caller                        
         J     EXIT                                                             
         DROP  AB,RB,RC                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
                                                                                
GOAWRKD  DSECT                     ** GOATSR S/R LOCAL W/S **                   
GOTSABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
GOAWRKL  EQU   *-GOAWRKD                                                        
                                                                                
ACBRA02  CSECT                                                                  
***********************************************************************         
* COMMON EXITS                                                        *         
***********************************************************************         
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    SR    RE,RE               SET CC LOW                                   
         J     *+8                                                              
EXITH    LA    RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LA    RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE ACVATICAND                                                     
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046ACBRA02   03/30/20'                                      
         END                                                                    
