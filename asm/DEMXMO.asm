*          DATA SET DEMXMO     AT LEVEL 014 AS OF 01/20/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMXMOA                                                                  
*INCLUDE DEBKUPDT                                                               
*INCLUDE SMTP                                                                   
*---------------------------------------------------------------------*         
* NATIONAL MINUTE BY MINUTE CONVERSION: OUTPUT PHASE.                 *         
*                                                                     *         
* IPHASE: DEMXMI                                                      *         
* OPHASE: DEMXMO                                                      *         
*                                                                     *         
* THIS CONVERSION OUTPUTS A FILE THAT INCLUDES NEW RECORDS, AS WELL   *         
* AS RECORDS TO BE DELETED. THE OUTPUT FILE WILL BE PASSED THROUGH A  *         
* POST-PROCESSOR (DEMRGICE), WHICH WILL MERGE THE DELETE RECORDS WITH *         
* THEIR REPLACEMENTS, WHERE APPLICABLE.                               *         
*---------------------------------------------------------------------*         
         TITLE '- DEMO CONVERSION - NATIONAL MINUTE BY MINUTE'                  
DEMXMO   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DEMXMO,RA                                            
         USING WORKD,RC            RC=A(TEMP W/S)                               
         USING DPRINT,R7           R7=PRINT                                     
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
*                                                                               
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD AFTER SORT)              
*                                                                               
         B     *+4(R1)                                                          
         B     CNV00               PROCESS A RECORD                             
         B     LASTHK              LAST TIME HOOK                               
*                                                                               
* PROCESS A RECORD                                                              
CNV00    MVI   PRINTSW,0                                                        
*                                                                               
         CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   CNV25                                                            
*                                                                               
         LHI   R0,MAXFILES         MAXIMUM NUMBER OF NTIFILS                    
         LA    RE,DELCNTS          COUNTER ARRAY                                
         ZAP   0(L'DELCNTS,RE),=P'0'   INIT EACH COUNTER                        
         LA    RE,L'DELCNTS(,RE)                                                
         BCT   R0,*-10                                                          
*                                                                               
         XC    UNKNTAB(UNKNTABL),UNKNTAB                                        
         XC    PREVTKEY,PREVTKEY                                                
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 VDEPRNT2                                                         
         MVC   P(25),=C'** STARTING OUTPUT PHASE:'                              
         GOTO1 VDEPRNT2                                                         
*                                                                               
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
*                                                                               
CNV25    MVI   FRSTREC,NO                                                       
*                                                                               
         BAS   RE,KNOWNSTA         SAVE UNKNOWN STATIONS TO UNKNTAB             
*                                                                               
         CLI   INTRTYP,IDRTPEQ     DELETION RECORD                              
         BNE   CNV30                                                            
         CLI   INTKEY+IDSRTPE-IDELKEY,IDSRTPEQ                                  
         BNE   CNV30                                                            
         BAS   RE,DELCNV                                                        
         B     CNVX                                                             
*                                                                               
CNV30    CLI   INTRTYP,MXMCODEQ    MINUTE BY MINUTE MAIN RECORD                 
         BNE   *+12                                                             
         BAS   RE,MXMCNV                                                        
         B     CNVX                                                             
*                                                                               
         CLI   INTRTYP,MPTCODEQ    PASSIVE FOR PROGRAM MINUTES                  
         BNE   *+12                                                             
         BAS   RE,MPTCNV                                                        
         B     CNVX                                                             
*                                                                               
         CLI   INTRTYP,MPDCODEQ    PASSIVE FOR POD DAYS/TIMES                   
         BNE   *+12                                                             
         BAS   RE,MPDCNV                                                        
         B     CNVX                                                             
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
* LAST TIME HOOK                                                                
LASTHK   DS    0H                                                               
         BAS   RE,RELJPTRS         RELEASE J-PNTRS (NIELSEN-DDS #S)             
*                                                                               
         IF (CP,DUPEXPSV,NE,=P'0') ANY SUPPRESSED DUP. EXT. PASSIVES?           
           EDIT  DUPEXPSV,WARNMS1#   YES: SEND WARNING E-MAIL                   
           GOTO1 VDATAMGR,DMCB,=C'OPMSG',('WARNMS1Q',WARNMSG1)                  
         ENDIF ,                                                                
*                                                                               
         BAS   RE,PRTCNTS          PRINT RECORD COUNTS                          
*                                                                               
         XC    DBUPARMS,DBUPARMS   WRITE EMAIL LINES TO EMTAPE                  
         LA    RE,DBUPARMS                                                      
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBUEMAIQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ  RELEASE                         
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   LSTH50                                                           
         XC    DBUPARMS,DBUPARMS        RELEASE CONTROL RECORDS                 
         LA    RE,DBUPARMS              TO CTAPE                                
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ  RELEASE                         
*                                                                               
LSTH50   BAS   RE,EMAILUNK         SEND EMAIL WITH UNKNOWN STATIONS,            
*                                  IF ANY                                       
LASTHKX  B     CNVX                                                             
*                                                                               
CNVX     XMOD1                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS DELETION RECORDS                                                      
***********************************************************************         
DELCNV   NTR1                                                                   
*                                                                               
         BAS   RE,UPDCBOOK         UPDATE CONTROL RECORDS WITH NEW BOOK         
*                                                                               
         BAS   RE,DELMXMR          DELETE MINUTE RECORDS                        
         BAS   RE,DELTPAS          DELETE 'T' PASSIVES                          
         BAS   RE,DELDPAS          DELETE 'D' PASSIVES                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS MINUTE RECORDS 'M'                                                    
***********************************************************************         
MXMCNV   NTR1                                                                   
*                                                                               
         MVC   THISKEY,INTKEY                                                   
         GOTO1 ABLDREC,DMCB,THISKEY                                             
         BAS   RE,BLDELEMS         BUILD VARIOUS ELEMENTS                       
         BAS   RE,PUTTAPE                                                       
*                                                                               
         BAS   RE,UMCOUNTS         UPDATE 'M' RECORD COUNTS                     
*                                                                               
         LA    R5,THISKEY          PRINT OUT MEDIA                              
         USING MXMKEY,R5                                                        
         CLC   MXMMEDIA,PRVMEDIA                                                
         BE    MXMC10                                                           
         MVC   P,SPACES                                                         
         MVC   P(32),=C'PROCESSING VIEWING STREAMS FOR *'                       
         CLI   MXMMEDIA,IMEDCAB                                                 
         BNE   *+14                                                             
         MVC   P+32(8),=C'CABLE* :'                                             
         B     MXMC05                                                           
         CLI   MXMMEDIA,IMEDBRO                                                 
         BE    *+6                 INVALID MEDIA                                
         DC    H'0'                                                             
         MVC   P+32(12),=C'BROADCAST* :'                                        
MXMC05   GOTO1 VDEPRNT2                                                         
         MVC   PRVMEDIA,MXMMEDIA                                                
*                                                                               
MXMC10   CLC   MXMSRC,PRVVTYPE     PRINT OUT VEWING TYPE                        
         BE    MXMC20                                                           
         MVC   P,SPACES                                                         
         MVC   P(L'SVVTYPE),SVVTYPE                                             
         GOTO1 VDEPRNT2                                                         
         MVC   PRVVTYPE,MXMSRC                                                  
         DROP  R5                                                               
*                                                                               
MXMC20   DS    0X                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS PASSIVE POINTERS FOR PROGRAM MINUTES 'T'                              
***********************************************************************         
MPTCNV   NTR1                                                                   
*                                                                               
         BAS   RE,UTCOUNT          UPDATE 'T' RECORD COUNTS                     
*                                                                               
         MVC   THISKEY,INTKEY                                                   
*                                                                               
         BAS   RE,DDSNUM           GET THE DDS(NTI) PROGRAM NUMBER              
         LA    R5,THISKEY                                                       
         USING MPTKEY,R5                                                        
         MVC   MPTPNUM,NTINUM                                                   
         DROP  R5                                                               
*                                                                               
         GOTO1 ABLDREC,DMCB,(C'E',THISKEY)   BUILD EXTENDED PASSIVE             
*                                                                               
* DEIS SAYS: WE SEE THAT OCCASIONALLY, SOME OF THE EXTENDED PASSIVES            
* PRODUCED BY THIS ROUTINE HAVE DUPLICATE MAJOR KEYS, BUT DIFFERENT             
* EXTENDED DATA. THIS IS PROBABLY (BUT NOT CERTAINLY) A BUG.                    
*                                                                               
* THERE IS ONLY ONE REASON THAT THIS BEHAVIOR COULD EVEN THEORETICALLY          
* BE INTENTIONAL, VIZ.: IF THE CODE WAS WRITTEN TO RELY ON THE DELDXMOD         
* "FEATURE" IN WHICH WE KEEP ONLY THE *FIRST* DUPLICATE PASSIVE. AS OF          
* NOV/2018, WE HAVE NEITHER THE TIME NOR THE INCLINATION TO INVESTIGATE         
* FURTHER TO DETERMINE WHETHER THE BEHAVIOR IS INTENTIONAL OR NOT.              
*                                                                               
* THE DUPLICATE PASSIVES COULD BE DUE TO ANY OF THESE:                          
*  1. BAD DATA FROM NIELSEN                                                     
*  2. LEGITIMATE DATA FROM NIELSEN THAT WE DIDN'T ACCOUNT FOR                   
*  3. A BUG IN THE OPEN SYSTEMS CALCULATION ENGINE                              
*  4. A BUG IN THE IPHASE OR OPHASE                                             
*  5. A DELIBERATE EXPLOITATION OF THE DELDXMOD "FEATURE" DESCRIBED             
*      ABOVE.                                                                   
*                                                                               
* REGARDLESS OF THE CAUSE, THE REAL ISSUE IS THAT WHILE DELDXMOD CAN            
* TOLERATE THE PRESENCE OF DUPLICATE PASSIVES, IDCAMS CANNOT. IN OTHER          
* WORDS, WHEN/IF THE NTIFILS ARE CONVERTED TO VSAM/IAM, WE WILL HAVE            
* TO GUARANTEE THAT THERE ARE NO DUPLICATE PASSIVE KEYS GOING INTO AN           
* IDCAMS UPDATE.                                                                
*                                                                               
* THEREFORE, THIS PROGRAM HAS BEEN MODIFIED TO DISCARD ALL BUT THE              
* FIRST DUPLICATE PASSIVE KEY OF THIS RECORD TYPE. IN ADDITION, WE SEND         
* OURSELVES AN AUTONOTE SO THAT WE KNOW THIS OCCURRED. I.E., THE                
* ORIGINAL BUG (ASSUMING IT IS A BUG) STILL EXISTS, BUT BY THROWING             
* AWAY ALL BUT THE FIRST OCCURRENCE OF EACH PASSIVE KEY, THE END RESULT         
* IS THAT WE ARE NO WORSE OFF THAN WE WERE BEFORE.                              
*                                                                               
         CLC   PREVTKEY,THISKEY    SAME AS PREVIOUS "T" KEY?                    
         BNE   MPTCNV10            NO: THAT'S OKAY                              
*                                                                               
         MVC   P(28),=CL28'DUPLICATE EXTENDED PASSIVE: '                        
         GOTO1 VHEXOUT,DMCB,THISKEY,P+28,23,=C'TOG'                             
         OC    DMCB+16(4),DMCB+16                                               
         JZ    *+2                 BAD RETURN FROM HEXOUT ?!?                   
         GOTO1 VDEPRNT2            TRACE THE DUPLICATE KEY IN SYSPRIN2          
         AP    DUPEXPSV,=P'1'      TALLY THE NUMBER OF DUPLICATES               
         B     MPTCNVX             SKIP THIS DUPLICATE EXTENDED PASSIVE         
*                                                                               
MPTCNV10 DS    0H                                                               
         MVC   PREVTKEY,THISKEY    SAVE THIS "T" KEY                            
*                                                                               
         BAS   RE,PUTTAPE                                                       
*                                                                               
MPTCNVX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS PASSIVE POINTERS FOR PODS DAY/TIMES 'D'                               
***********************************************************************         
MPDCNV   NTR1                                                                   
*                                                                               
         BAS   RE,UDCOUNT          UPDATE 'D' RECORD COUNTS                     
*                                                                               
         MVC   THISKEY,INTKEY                                                   
*                                                                               
         BAS   RE,DDSNUM           GET THE DDS(NTI) PROGRAM NUMBER              
         LA    R5,THISKEY                                                       
         USING MPDKEY,R5                                                        
         MVC   MPDNTI,NTINUM                                                    
         DROP  R5                                                               
*                                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY)   BUILD PASSIVE RECORD               
         BAS   RE,PUTTAPE                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
**********************************************************************          
* UPDATE 'M' RECORD COUNTS                                                      
**********************************************************************          
UMCOUNTS DS    0X                                                               
         ICM   R1,15,MFILCNT       UPDATE FILE COUNT                            
         AHI   R1,1                                                             
         STCM  R1,15,MFILCNT                                                    
*                                                                               
         L     RF,AOREC                                                         
         LA    RF,4(RF)                                                         
         CLC   PREVMAJK,0(RF)      UPDATE DIRECTORY COUNT                       
         BE    UMCOUNTX                                                         
         ICM   R1,15,MDIRCNT                                                    
         AHI   R1,1                                                             
         STCM  R1,15,MDIRCNT                                                    
         MVC   PREVMAJK,0(RF)      SAVE PREVIOUS MAJOR KEY                      
*                                                                               
UMCOUNTX BR    RE                                                               
**********************************************************************          
* UPDATE 'D' PASSIVE RECORD COUNT                                               
**********************************************************************          
UDCOUNT DS     0X                                                               
         ICM   RF,15,DPASCNT                                                    
         AHI   RF,1                                                             
         STCM  RF,15,DPASCNT                                                    
         BR    RE                                                               
**********************************************************************          
* UPDATE 'T' PASSIVE RECORD COUNT                                               
**********************************************************************          
UTCOUNT DS     0X                                                               
         ICM   RF,15,TPASCNT                                                    
         AHI   RF,1                                                             
         STCM  RF,15,TPASCNT                                                    
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DELETE MINUTE RECORDS                                                         
***********************************************************************         
DELMXMR  NTR1                                                                   
*                                                                               
         LA    R6,INTKEY                                                        
         USING IDELKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING MXMKEY,R5                                                        
         MVI   MXMCODE,MXMCODEQ                                                 
         MVC   MXMBOOK,IDBOOK                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   IDMEDIA,0                                                        
         BE    DELM10                                                           
         MVC   MXMMEDIA,IDMEDIA    IF SPECIFIC MEDIA, USE IT                    
         B     DELM20                                                           
*                                                                               
DELM10   LA    R2,ALLMEDTB         IF NO MEDIA GIVEN, NEED TO DELETE            
DELM15   CLI   0(R2),X'FF'         ALL MEDIAS                                   
         BE    DELMX                                                            
         MVC   MXMMEDIA,0(R2)                                                   
*                                                                               
DELM20   LA    R3,ALLSRCTB         DELETE ALL SOURCES (I.E. VIEWING             
DELM25   CLI   0(R3),X'FF'         TYPES) ONE BY ONE                            
         BNE   DELM30                                                           
         OR    R2,R2               ARE WE GOING THROUGH ALL MEDIAS?             
         BZ    DELMX               NO. DONE                                     
         LA    R2,1(R2)                                                         
         B     DELM15              YES. GO TO NEXT MEDIA                        
*                                                                               
DELM30   MVC   MXMSRC,0(R3)                                                     
*                                                                               
         MVI   MXMBTYP,0                                                        
         CLI   MXMMEDIA,IMEDBRO                                                 
         BNE   *+8                                                              
         MVI   MXMBTYP,C'A'        BOOKTYPE FOR BROADCAST                       
*                                                                               
         MVC   MXMSTAT,IDSTATN     SPECIFIC STATION OR ZEROES                   
*                                                                               
         BAS   RE,DHIGH            READ HIGH ON DIRECTORY                       
DELM40   BNE   DELM100             ERROR OR EOF                                 
         CLC   KEYFOUND(MXMSTAT-MXMKEY),KEY    SAME KEY UP TO STATION?          
         BNE   DELM100                         NO. STOP                         
         OC    MXMSTAT,MXMSTAT                                                  
         BZ    DELM50                                                           
         CLC   MXMSTAT,KEYFOUND+MXMSTAT-MXMKEY  REQUESTED STATION?              
         BNE   DELM100                          NO. STOP                        
*                                                                               
DELM50   L     RF,AWREC            FLAG RECORD AS DELETE AND WRITE              
         LA    RF,4(RF)             IT TO THE OUTPUT FILE                       
         L     RE,AOREC            RF->DIRECTORY RECORD JUST READ               
         XC    0(4,RE),0(RE)                                                    
         LA    RE,4(RE)            RE->OUTPUT RECORD                            
         MVC   0(18,RE),0(RF)      MOVE MAJOR KEY TO I/O AREA                   
         MVC   18(4,RE),=X'0000FFFF'  TREAT IT LIKE A PASSIVE                   
         MVC   22(1,RE),18(RF)     AND STATUS BYTE                              
         OI    22(RE),X'80'        SET DELETE BIT ON                            
         LA    RE,23(RE)           RE POINTS JUST BEYOND THE POINTER            
*                                                                               
         LLC   RF,18(RF)           PICK UP STATUS BYTE                          
         NILF  GRF,X'0000003F'     ISOLATE THE LOGICAL FILE NUMBER              
         SHI   RF,1                                                             
         JM    *+2                 CORRUPTED LOGICAL FILE NUMBER!               
         MHI   RF,L'DELCNTS        RF = INDEX INTO COUNTER ARRAY                
         LA    RF,DELCNTS(RF)      RF = A(THIS FILE'S COUNTER)                  
         AP    0(L'DELCNTS,RF),=P'1'  INCREMENT COUNTER                         
*                                                                               
         L     RF,AOREC            SET OUTPUT RECORD LENGTH                     
         SR    RE,RF                                                            
         STCM  RE,3,0(RF)                                                       
*                                                                               
         BAS   RE,PUTTAPE          OUPUT RECORD MARKED FOR DELETION             
*                                                                               
         BAS   RE,UMCOUNTS         UPDATE 'M' RECORD COUNTS                     
*                                                                               
         BAS   RE,DSEQ                                                          
         B     DELM40                                                           
*                                                                               
DELM100  LA    R3,1(R3)                                                         
         B     DELM25              GO PROCESS NEXT VIEWING TYPE                 
*                                                                               
DELMX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE  'T' PASSIVES                                                          
***********************************************************************         
DELTPAS  NTR1                                                                   
*                                                                               
         LA    R6,INTKEY                                                        
         USING IDELKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING MPTKEY,R5                                                        
         MVI   MPTCODE,MPTCODEQ                                                 
         MVC   MPTBOOK,IDBOOK                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   IDMEDIA,0                                                        
         BE    DELT10                                                           
         MVC   MPTMEDIA,IDMEDIA    IF SPECIFIC MEDIA, USE IT                    
         B     DELT20                                                           
*                                                                               
DELT10   LA    R2,ALLMEDTB         IF NO MEDIA GIVEN, NEED TO DELETE            
DELT15   CLI   0(R2),X'FF'         ALL MEDIAS                                   
         BE    DELTX                                                            
         MVC   MPTMEDIA,0(R2)                                                   
*                                                                               
DELT20   LA    R3,ALLSRCTB         DELETE ALL SOURCES (I.E. VIEWING             
DELT25   CLI   0(R3),X'FF'         TYPES) ONE BY ONE                            
         BNE   DELT30                                                           
         OR    R2,R2               ARE WE GOING THROUGH ALL MEDIAS?             
         BZ    DELTX               NO. DONE                                     
         LA    R2,1(R2)                                                         
         B     DELT15              YES. GO TO NEXT MEDIA                        
*                                                                               
DELT30   MVC   MPTSRC,0(R3)                                                     
*                                                                               
         MVI   MPTBTYP,0                                                        
         CLI   MPTMEDIA,IMEDBRO                                                 
         BNE   *+8                                                              
         MVI   MPTBTYP,C'A'        BOOKTYPE FOR BROADCAST                       
*                                                                               
         MVC   MPTSTAT,IDSTATN     SPECIFIC STATION OR ZEROES                   
*                                                                               
         BAS   RE,DHIGH            READ HIGH ON DIRECTORY                       
DELT40   BNE   DELT100             ERROR OR EOF                                 
         CLC   KEYFOUND(MPTSTAT-MPTKEY),KEY    SAME KEY UP TO STATION?          
         BNE   DELT100                         NO. STOP                         
         OC    MPTSTAT,MPTSTAT                                                  
         BZ    DELT50                                                           
         CLC   MPTSTAT,KEYFOUND+MPTSTAT-MPTKEY  REQUESTED STATION?              
         BNE   DELT100                          NO. STOP                        
*                                                                               
DELT50   L     RF,AWREC            FLAG RECORD AS DELETE AND WRITE              
         LA    RF,4(RF)            IT TO THE OUTPUT FILE                        
         L     RE,AOREC            RF->DIRECTORY RECORD JUST READ               
         XC    0(4,RE),0(RE)                                                    
         LA    RE,4(RE)            RE->OUTPUT RECORD                            
         MVC   0(18,RE),0(RF)      MOVE MAJOR KEY TO I/O AREA                   
         MVC   18(4,RE),19(RF)     DATA AREA                                    
         MVC   22(1,RE),18(RF)     AND STATUS BYTE                              
         OI    22(RE),X'80'        SET DELETE BIT ON                            
         LA    RE,23(RE)           RE POINTS JUST BEYOND THE POINTER            
*                                                                               
         L     RF,AOREC            SET OUTPUT RECORD LENGTH                     
         SR    RE,RF                                                            
         STCM  RE,3,0(RF)                                                       
*                                                                               
         BAS   RE,PUTTAPE          OUPUT RECORD MARKED FOR DELETION             
*                                                                               
         BAS   RE,UTCOUNT          UPDATE 'T' RECORD COUNTS                     
*                                                                               
         BAS   RE,DSEQ                                                          
         B     DELT40                                                           
*                                                                               
DELT100  LA    R3,1(R3)                                                         
         B     DELT25              GO PROCESS NEXT VIEWING TYPE                 
*                                                                               
DELTX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE 'D' PASSIVES                                                           
***********************************************************************         
DELDPAS  NTR1                                                                   
*                                                                               
         LA    R6,INTKEY                                                        
         USING IDELKEY,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING MPDKEY,R5                                                        
         MVI   MPDCODE,MPDCODEQ                                                 
         MVC   MPDBOOK,IDBOOK                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   IDMEDIA,0                                                        
         BE    DELD10                                                           
         MVC   MPDMEDIA,IDMEDIA    IF SPECIFIC MEDIA, USE IT                    
         B     DELD20                                                           
*                                                                               
DELD10   LA    R2,ALLMEDTB         IF NO MEDIA GIVEN, NEED TO DELETE            
DELD15   CLI   0(R2),X'FF'         ALL MEDIAS                                   
         BE    DELDX                                                            
         MVC   MPDMEDIA,0(R2)                                                   
*                                                                               
DELD20   LA    R3,ALLSRCTB         DELETE ALL SOURCES (I.E. VIEWING             
DELD25   CLI   0(R3),X'FF'         TYPES) ONE BY ONE                            
         BNE   DELD30                                                           
         OR    R2,R2               ARE WE GOING THROUGH ALL MEDIAS?             
         BZ    DELDX               NO. DONE                                     
         LA    R2,1(R2)                                                         
         B     DELD15              YES. GO TO NEXT MEDIA                        
*                                                                               
DELD30   MVC   MPDSRC,0(R3)                                                     
*                                                                               
         MVI   MPDBTYP,0                                                        
         CLI   MPDMEDIA,IMEDBRO                                                 
         BNE   *+8                                                              
         MVI   MPDBTYP,C'A'        BOOKTYPE FOR BROADCAST                       
*                                                                               
         MVC   MPDSTAT,IDSTATN     SPECIFIC STATION OR ZEROES                   
*                                                                               
         BAS   RE,DHIGH            READ HIGH ON DIRECTORY                       
DELD40   BNE   DELD100             ERROR OR EOF                                 
         CLC   KEYFOUND(MPDSTAT-MPDKEY),KEY    SAME KEY UP TO STATION?          
         BNE   DELD100                         NO. STOP                         
         OC    MPDSTAT,MPDSTAT                                                  
         BZ    DELD50                                                           
         CLC   MPDSTAT,KEYFOUND+MPDSTAT-MPDKEY  REQUESTED STATION?              
         BNE   DELD100                          NO. STOP                        
*                                                                               
DELD50   L     RF,AWREC            FLAG RECORD AS DELETE AND WRITE              
         LA    RF,4(RF)            IT TO THE OUTPUT FILE                        
         L     RE,AOREC            RF->DIRECTORY RECORD JUST READ               
         XC    0(4,RE),0(RE)                                                    
         LA    RE,4(RE)            RE->OUTPUT RECORD                            
         MVC   0(18,RE),0(RF)      MOVE MAJOR KEY TO I/O AREA                   
         XC    18(2,RE),18(RE)     D/A AREA                                     
         MVC   20(2,RE),=X'FFFF'   PASSIVE POINTER INDICATOR                    
         MVC   22(1,RE),18(RF)     AND STATUS BYTE                              
         OI    22(RE),X'80'        SET DELETE BIT ON                            
         LA    RE,23(RE)           RE POINTS JUST BEYOND THE POINTER            
*                                                                               
         L     RF,AOREC            SET OUTPUT RECORD LENGTH                     
         SR    RE,RF                                                            
         STCM  RE,3,0(RF)                                                       
*                                                                               
         BAS   RE,PUTTAPE          OUPUT RECORD MARKED FOR DELETION             
*                                                                               
         BAS   RE,UDCOUNT          UPDATE 'D' RECORD COUNTS                     
*                                                                               
         BAS   RE,DSEQ                                                          
         B     DELD40                                                           
*                                                                               
DELD100  LA    R3,1(R3)                                                         
         B     DELD25              GO PROCESS NEXT VIEWING TYPE                 
*                                                                               
DELDX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD DEMO ELEMENTS FOR THE 'M' RECORD IN OREC                                
***********************************************************************         
BLDELEMS NTR1                                                                   
*                                                                               
         LA    R6,TEMP             X'01' MARKET ELEMENT                         
         XC    TEMP,TEMP                                                        
         USING MARELEM,R6                                                       
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             X'08' DATA TYPE ELEMENT                      
         XC    TEMP,TEMP                                                        
         USING DTELEM,R6                                                        
         MVI   DTCODE,DTCODEQ                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,DMCB+4           LENGTH OF TABLE ENTRY                        
*                                                                               
         USING VWDESCD,R1                                                       
BLDE10   CLC   INTKSRC,VWDSRC                                                   
         BE    BLDE20                                                           
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     BLDE10                                                           
*                                                                               
BLDE20   ZIC   RE,VWDLEN                                                        
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTTYPE(0),VWDSCP                                                 
         DROP  R1                                                               
*                                                                               
         AHI   RE,DTLENQ+1                                                      
         STC   RE,DTLEN                                                         
*                                                                               
         MVC   SVVTYPE,DTTYPE      SAVE VIEWING TYPE FOR PRINTING               
*                                                                               
         GOTO1 APUTEL,DTELEM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             X'09' MINUTE ELEMENT                         
         XC    TEMP,TEMP                                                        
         USING MNTELEM,R6                                                       
         MVI   MNTCODE,MNTCODEQ                                                 
         MVC   MNTMIN,INTMIN                                                    
         MVC   MNTMOP,INTMOP                                                    
         MVC   MNTCTNUM,INTCTNUM                                                
         MVC   MNTCOMM,INTCOMM                                                  
         MVI   MNTLEN,MNTLEN1Q                                                  
         CLI   MNTCOMM,MNTCOMMC    FIELDS BELOW ARE ONLY FOR                    
         BNE   BLDEL30             MINUTES WITH COMMERCIAL CONTENT              
         MVI   MNTLEN,MNTLEN2Q                                                  
         MVC   MNTCMSEC,INTCMSEC                                                
         MVC   MNTPRSEC,INTPRSEC                                                
         MVC   MNTPSSEC,INTPSSEC                                                
         MVC   MNTPODST,INTPODST                                                
         MVC   MNTPODMN,INTPODMN                                                
         MVC   MNTPODSC,INTPODSC                                                
         MVC   MNTPODNO,INTPODNO                                                
         MVC   MNTFL,INTFL                                                      
         MVC   MNTPODTS,INTPODTS                                                
BLDEL30  GOTO1 APUTEL,MNTELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             X'10' PROGRAM TYPE ELEMENT                   
         XC    TEMP,TEMP                                                        
         USING PHTELEM,R6                                                       
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
         MVC   PHTPTYP,INTPTYP                                                  
*                                                                               
         CLC   INTSBTYP,=C'    '   INSERT SUBPROGRAM TYPE IF PRESENT            
         BE    *+16                                                             
         MVC   PHTSPTYP,INTSBTYP                                                
         MVC   PHTPTYP4,INTPTYP    AND LONG PROGRAM TYPE                        
*                                                                               
         MVC   PHTPREM,INTPREM                                                  
         MVC   PHTSCNT,INTSTAC                                                  
         MVC   PHTCOVR,INTCOV                                                   
         MVC   PHTRSF,INTBREAK                                                  
         MVC   PHTDPT2,INTDPT                                                   
         MVC   PHTNTI,INTPNUM                                                   
*                                                                               
         BAS   RE,DDSNUM                                                        
         MVC   PHTDDS,NTINUM       GET DDS (NTI) PROGRAM NUMBER                 
*                                                                               
         MVC   PHTINDS,INTFLAGS                                                 
*                                                                               
         GOTO1 APUTEL,PHTELEM                                                   
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             X'21' PROGRAM NAME ELEMENT                   
         XC    TEMP,TEMP                                                        
         USING PPNELEM,R6                                                       
         MVI   PPNCODE,PPNCODEQ                                                 
         MVC   PPNNME(L'INTPNAME),INTPNAME                                      
         OC    PPNNME(L'INTPNAME),SPACES                                        
         CLC   PPNNME(L'INTPNAME),SPACES                                        
         BE    BLDEL40                                                          
         LA    R5,PPNNME+L'INTPNAME-1                                           
         LA    R1,L'INTPNAME                                                    
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,PPNNME-PPNELEM(R1)                                            
         STC   R1,PPNELN                                                        
         GOTO1 APUTEL,PPNELEM                                                   
         DROP  R6                                                               
*                                                                               
BLDEL40  LA    R6,TEMP             X'22' PROGRAM RUN TIME ELEMENT               
         XC    TEMP,TEMP                                                        
         USING NTELEM,R6                                                        
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ2                                                   
         MVC   NTSTIM,INTSTIM                                                   
         MVC   NTETIM,INTETIM                                                   
         MVC   NTSQH,INTSQH                                                     
         MVC   NTEQH,INTEQH                                                     
*                                                                               
         LHI   R0,X'80'                                                         
         ZIC   R1,INTDAY                                                        
         SRL   R0,1                                                             
         BCT   R1,*-4                                                           
         STC   R0,NTDAY            WEEK DAY BITS. MON=X'40'...SUN=X'01'         
*                                                                               
         MVC   NTFEED,INTFEED                                                   
         MVC   NTAUDES,INTAUDES                                                 
         MVC   NTTNUM,INTTNUM                                                   
         MVC   NTLIVE,INTLIVE                                                   
         MVC   NTGAP,INTGAPD                                                    
         MVC   NTDUR2,INTDURM                                                   
*                                                                               
         GOTO1 APUTEL,NTELEM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,TEMP             X'24' EPISODE TITLE ELEMENT                  
         XC    TEMP,TEMP                                                        
         USING PPTELEM,R6                                                       
         MVI   PPTCODE,PPTCODEQ                                                 
         MVC   PPTTITLE(L'INTEPNAM),INTEPNAM                                    
         OC    PPTTITLE(L'INTEPNAM),SPACES                                      
         CLC   PPTTITLE(L'INTEPNAM),SPACES                                      
         BE    BLDEL50                                                          
         LA    R5,PPTTITLE+L'INTEPNAM-1                                         
         LA    R1,L'INTEPNAM                                                    
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,PPTTITLE-PPTELEM(R1)                                          
         STC   R1,PPTELN                                                        
         GOTO1 APUTEL,PPTELEM                                                   
         DROP  R6                                                               
*                                                                               
BLDEL50  OC    INTRPRDT,INTRPRDT   X'25' CORRECTION ELEMENT                     
         BZ    BLDEL60                                                          
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING CRRELEM,R6                                                       
         MVI   CRRCODE,CRRCODEQ                                                 
         MVI   CRRLEN,CRRLEN2Q                                                  
         MVC   CRRDATE,INTRPRDT                                                 
         MVC   CRRRTYP,INTRPTYP                                                 
         GOTO1 APUTEL,CRRELEM                                                   
         DROP  R6                                                               
*                                                                               
BLDEL60  OC    INTTRK,INTTRK       X'28' TRACKAGE ELEMENT                       
         BZ    BLDEL70                                                          
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING NTRKELEM,R6                                                      
         MVI   NTRKCODE,NTRKCDEQ                                                
         MVC   NTRKNUM,INTTRK                                                   
         MVC   NTRKNAME(L'INTTRNAM),INTTRNAM                                    
         OC    NTRKNAME(L'INTTRNAM),SPACES                                      
         LA    R5,NTRKNAME+L'INTTRNAM-1                                         
         LA    R1,L'INTTRNAM                                                    
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,NTRKNAME-NTRKELEM(R1)                                         
         STC   R1,NTRKLN                                                        
         GOTO1 APUTEL,NTRKELEM                                                  
         DROP  R6                                                               
*                                                                               
BLDEL70  LA    R5,DBLOCKA          BUILD DEMO ELEMENTS                          
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,INTBOOK                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         XC    WORK(10),WORK                                                    
         CLI   INTMEDIA,IMEDBRO   BROADCAST                                     
         BNE   BLDEL74                                                          
         MVC   WORK(7),BFORMAT    SET UP OUTPUT FORMAT BLOCK FOR BROD           
         B     BLDEL76                                                          
BLDEL74  CLI   INTMEDIA,IMEDCAB   CABLE                                         
         BE    *+6                                                              
         DC    H'0'               INVALID KEY MEDIA                             
         MVC   WORK(7),CFORMAT    SET UP OUTPUT FORMAT BLOCK FOR CABLE          
*                                                                               
BLDEL76  MVC   WORK+7(2),INTBOOK  TO FORCE BOOK ON X'5E' ELEMENT                
*                                                                               
         LA    RE,CONDWORK                                                      
         LA    RF,CONWRKLN                                                      
         XCEF                                                                   
*                                                                               
         LA    RE,CONDWORK                                                      
         LA    RF,MAXDIDEM*4                                                    
         LA    R2,INTACCS                                                       
         LR    R3,RF                                                            
         MVCL  RE,R2               MOVE DEMOS TO CONDWORK                       
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         ICM   RF,15,CDEMEL                                                     
         DROP  RF                                                               
         GOTO1 (RF),DMCB,(C'C',WORK),DBLOCKD,CONDWORK                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    BLDEL100                                                         
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                  ADD DEMO ELEMENTS TO RECORD                  
BLDEL80  CLI   0(R1),0                                                          
         BE    BLDEL100                                                         
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDEL80                                                          
                                                                                
BLDEL100 DS    0X                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE DDS (NTI) NUMBER FOR THE NIELSEN PROGRAM NUMBER IN INTPNM10           
* RETURN THE DDS (NTI) NUMBER IN NTINUM.                                        
***********************************************************************         
DDSNUM   NTR1                                                                   
*                                                                               
         CLI   INTMEDIA,IMEDBRO    BROADCAST                                    
         BNE   DDSNCAB                                                          
*                                                                               
         CLI   FRSTDDSB,YES        FIRST TIME IN DDSNUM FOR BROADCAST           
         BNE   DDSNB10                                                          
         MVI   FRSTDDSB,NO         BUILD PROGRAM NUMBER MAP                     
*                                                                               
         MVC   ASREC,AWREC         USE WREC FOR NTIPRG                          
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'N',VBITMAP1),0,0                        
         ST    R9,ASREC            RESTORE ASREC                                
*                                                                               
DDSNB10  MVC   ASREC,AWREC         USE WREC FOR NTIPRG                          
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),INTPNM10,0                    
         ST    R9,ASREC            RESTORE ASREC                                
         MVC   NTINUM,0(R1)        DDS (NTI) INTERNAL NUMBER                    
         B     DDSNUMX                                                          
*                                                                               
DDSNCAB  CLI   INTMEDIA,IMEDCAB    CABLE                                        
         BE    *+6                                                              
         DC    H'0'                INVALID KEY MEDIA                            
*                                                                               
         LA    R5,KEY              READ 'J' PASSIVES FOR PRG NUMBERS            
         XC    KEY,KEY                                                          
         USING PJKEY,R5                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT,INTSTA                                                    
         MVC   PJEXTNUM,INTPNUM    NIELSEN PROGRAM NUMBER                       
         BAS   RE,DHIGH                                                         
         BNE   DDSNC05                                                          
         XC    NTINUM,NTINUM                                                    
         CLC   KEYFOUND(PJINTNUM-PJKEY),KEY    NTI-DDS NUMBER FOUND?            
         BNE   DDSNC05                                                          
         MVC   NTINUM,KEYFOUND+PJINTNUM-PJKEY  YES                              
         B     DDSNUMX                                                          
*                                                                               
DDSNC05  MVC   PJEXTNUM,=5X'00'    GET NEXT NUMBER AVAILABLE                    
         BAS   RE,DHIGH                                                         
         BNE   DDSNC08             ERROR OR EOF                                 
         CLC   KEYFOUND(PJINTNUM-PJKEY),KEY    SAME STATION/BOOK?               
         BNE   DDSNC08                                                          
         MVC   NTINUM,KEYFOUND+PJINTNUM-PJKEY  LATEST DDS PRG NUMBER            
         XC    NTINUM,=X'FFFF'                                                  
*                                                                               
DDSNC08  L     R1,=A(NTITBL)       NIELSEN-TO-DDS PROGRAM TABLE                 
         XC    DMCB(4),DMCB                                                     
         USING NTITBLD,R1                                                       
DDSNC10  OC    NTSTAT,NTSTAT       SEARCH TABLE FOR NIELSEN PRG NUMBER          
         BZ    DDSNC30                                                          
         CLC   NTSTAT,=5X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                NTITBL OVERFLOW                              
         CLC   NTSTAT,INTSTA       COMPARE ON STATION                           
         BNE   DDSNC20                                                          
         CLC   NTNIELSN,INTPNUM    SAME NIELSEN PRG NUMBER?                     
         BNE   *+14                                                             
         MVC   NTINUM,NTDDS        YES. USE THE DDS NUMBER ASSIGNED             
         B     DDSNUMX                                                          
         CLC   NTNIELSN,=5X'FF'    IS THIS THE HIGHEST DDS# USED?               
         BNE   DDSNC20                                                          
         MVC   NTINUM,NTDDS        YES. SAVE IT FOR NOW                         
         ST    R1,DMCB             SAVE ADR OF 'FF' ENTRY IN TABLE              
DDSNC20  LA    R1,NTITBLDL(R1)     NOT FOUND                                    
         B     DDSNC10                                                          
*                                                                               
DDSNC30  SR    RE,RE                                                            
         ICM   RE,3,NTINUM         HIGHEST DDS NUMBER USED                      
         LA    RE,1(RE)                                                         
         STH   RE,NTINUM           NEXT AVAILABLE DDS NUMBER                    
         MVC   NTSTAT,INTSTA       SAVE STATION                                 
         MVC   NTNIELSN,INTPNUM    SAVE NIELSEN PROGRAM NUMBER                  
         MVC   NTDDS,NTINUM        SAVE DDS PRG NUMBER                          
         LA    R1,NTITBLDL(R1)                                                  
*                                                                               
         OC    DMCB,DMCB           UPDATE HIGHEST DDS# USED                     
         BZ    *+8                                                              
         L     R1,DMCB             GET ADDR OF 'FF' IN TABLE                    
         MVC   NTSTAT,INTSTA       SAVE STATION                                 
         MVC   NTNIELSN,=5X'FF'    LATEST DDS NUMBER FOR STATION                
         MVC   NTDDS,NTINUM        SAVE DDS PRG NUMBER                          
*                                                                               
         DROP  R1,R5                                                            
*                                                                               
DDSNUMX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RELEASE J-POINTERS THAT MAKE THE TRANSLATION BETWEEN THE NIELSEN              
* PROGRAM NUMBERS AND DDS (NTI) PROGRAM NUMBERS.                                
***********************************************************************         
RELJPTRS NTR1                                                                   
*                                                                               
* RELEASE UPDATED POINTERS AND BIT MAP FOR BROADCAST                            
RELJBROD CLI   FRSTDDSB,YES                                                     
         BE    RELJCAB             NO BROADCAST DATA,SKIP                       
         DS    0H                  RELEASE UPDATED BIT MAP                      
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         DS    0H                  GENERATE J-RECS                              
         GOTO1 VNTIPRG,DMCB,=C'JREC',(0,VBITMAP1),0                             
*                                                                               
* RELEASE UPDATED POINTERS FOR CABLE                                            
RELJCAB  L     R5,=A(NTITBL)       TABLE OF NIELSEN-TO-DDS PRG NUMBERS          
         USING NTITBLD,R5                                                       
RELJ10   OC    NTSTAT,NTSTAT       IS TABLE EMPTY?                              
         BZ    RELJX               YES. EXIT                                    
         LA    R6,THISKEY          LOOK UP NTI PGR CODE PASSIVE RECD            
         XC    THISKEY,THISKEY                                                  
         USING PJKEY,R6                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT,NTSTAT       STATION                                      
         MVC   PJINTNUM(2),NTDDS   DDS NUMBER                                   
         CLC   NTNIELSN,=5X'FF'    MAX DDS NUMBER RECORD                        
         BNE   *+14                                                             
         XC    PJINTNUM(2),=X'FFFF'   FORCE LATEST DDS NUMBER FIRST             
         B     *+10                                                             
         MVC   PJEXTNUM,NTNIELSN   NIELSEN NUMBER                               
         GOTO1 ABLDREC,DMCB,(C'P',PJKEY)                                        
         BAS   RE,PUTTAPE                                                       
         LA    R5,NTITBLDL(R5)                                                  
         B     RELJ10                                                           
         DROP  R5,R6                                                            
*                                                                               
RELJX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*PUTTAPE: ROUTINE TO PUT RECORD TO OUTPUT TAPE                                  
*         THIS ROUTINE ALSO CALLS DEBKUPDT TO ADD A BOOK FOR EVERY              
*         ACTIVE RECORD RELEASED TO TAPE.                                       
**********************************************************************          
PUTTAPE  NTR1                                                                   
*                                                                               
         GOTO1 APUTTAPE                                                         
*                                                                               
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),MXMCODEQ      ONLY FOR THE MINUTE RECORDS                  
         BNE   PUTTAPEX                                                         
         BAS   RE,UPDCBOOK         UPDATE CONTROL RECORDS WITH NEW BK           
*                                                                               
PUTTAPEX B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CALL DEBKUPDT TO KEEP TRACK OF THE BOOKS BEING UPDATED.                       
**********************************************************************          
UPDCBOOK NTR1                                                                   
         XC    DBUPARMS,DBUPARMS                                                
         LA    R3,DBUPARMS                                                      
         USING DBUPARMD,R3                                                      
*                                                                               
         MVC   DBUACOMS,ACOMFACS   A(COMFACS)                                   
         MVI   DBURTYP,DBURNETQ                                                 
         MVC   DBUBOOK,INTBOOK     BOOK                                         
         MVI   DBUBKFLG,DBUBKWQ    WEEKLY BOOK                                  
*                                                                               
         CLI   INTMEDIA,IMEDBRO                                                 
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPNETQ   POSTING TYPE FOR BROADCAST                   
         CLI   INTMEDIA,IMEDCAB                                                 
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPCBLQ   POSTING TYPE FOR CABLE                       
*                                                                               
         CLI   INTMEDIA,0          IF ALL MEDIAS                                
         BNE   UPDC20                                                           
         MVI   DBUPSTYP,DBUPNETQ   START WITH BROADCAST                         
*                                                                               
UPDC20   GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,ADDQ    ADD DEMO UPDATE BOOK          
*                                                                               
         CLI   INTMEDIA,0          IF ALL MEDIAS                                
         BNE   UPDCBKX                                                          
         CLI   DBUPSTYP,DBUPNETQ                                                
         BNE   UPDCBKX                                                          
         MVI   DBUPSTYP,DBUPCBLQ   CONTINUE WITH CABLE                          
         B     UPDC20                                                           
*                                                                               
UPDCBKX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT RECORD COUNTS BY FILE AND DIRECTORY                                     
**********************************************************************          
PRTCNTS  NTR1                                                                   
*                                                                               
         GOTO1 VDEPRNT2            BLANK LINE                                   
         MVC   P(13),=C'RECORD COUNTS'                                          
         GOTO1 VDEPRNT2                                                         
         MVC   P(13),=C'-------------'                                          
         GOTO1 VDEPRNT2                                                         
*                                                                               
         MVC   P(26),=CL26'TOTAL OUTPUT RECORD COUNT:'                          
         ICM   RE,15,MFILCNT                                                    
         ICM   RF,15,DPASCNT                                                    
         AR    RE,RF                                                            
         ICM   RF,15,TPASCNT                                                    
         AR    RE,RF                                                            
         EDIT  (RE),(12,P+26),ZERO=NOBLANK,COMMAS=YES                           
         GOTO1 VDEPRNT2                                                         
*                                                                               
         MVC   P(26),=CL26'DIRECTORY RECORD COUNT:'                             
         ICM   RE,15,MDIRCNT                                                    
         ICM   RF,15,DPASCNT                                                    
         AR    RE,RF                                                            
         ICM   RF,15,TPASCNT                                                    
         AR    RE,RF                                                            
         EDIT  (RE),(12,P+26),ZERO=NOBLANK,COMMAS=YES                           
         GOTO1 VDEPRNT2                                                         
*                                                                               
         MVC   P(26),=CL26'PASSIVES RECORD COUNT:'                              
         ICM   RE,15,DPASCNT                                                    
         ICM   RF,15,TPASCNT                                                    
         AR    RE,RF                                                            
         EDIT  (RE),(12,P+26),ZERO=NOBLANK,COMMAS=YES                           
         GOTO1 VDEPRNT2                                                         
*                                                                               
         MVC   P(24),=C'DELETED MAJOR KEY COUNT:'                               
         GOTO1 VDEPRNT2                                                         
         LHI   R0,MAXFILES                                                      
         DO    FROM=(R0)                                                        
           LR    RE,R0                                                          
           SHI   RE,MAXFILES                                                    
           LPR   RE,RE                                                          
           LR    R1,RE             R1 = THIS LOGICAL FILE #                     
           MHI   RE,L'DELCNTS      RE = DISPLACEMENT TO THIS FILE'S CTR         
           LA    RE,DELCNTS(RE)    RE = A(THIS FILE'S COUNTER)                  
           IF (CP,0(L'DELCNTS,RE),NE,=P'0')                                     
             MHI   R1,L'NTIFILS                                                 
             LA    R1,NTIFILS(R1)  R1 = A(NTIDIR TABLE ENTRY)                   
             MVC   P(7),0(R1)      NTIFIL NAME                                  
             EDIT  (P6,0(RE)),(12,P+10),COMMAS=YES                              
             GOTO1 VDEPRNT2                                                     
           ENDIF ,                                                              
         ENDDO ,                                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DATAMGR ROUTINE: READ HIGH ON DIRECTORY                                       
* SET CC NOT EQUAL IF ERROR OR END OF FILE                                      
**********************************************************************          
DHIGH    NTR1                                                                   
         L     R6,AWREC            READ WITH 'KEY' INTO WREC                    
         LA    R6,4(R6)                                                         
         XC    0(L'KEY,R6),0(R6)                                                
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',KEY,(R6)                    
*                                                                               
         CLI   8(R1),0                                                          
         BNE   DHIGHERR            ERROR OR EOF                                 
*                                                                               
         L     RE,AWREC                                                         
         LA    RE,4(RE)                                                         
         MVC   KEYFOUND,0(RE)                                                   
         B     DHIGHOK                                                          
*                                                                               
DHIGHOK  CR    RE,RE                                                            
         B     XIT                                                              
DHIGHERR CHI   RB,0                                                             
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DATAMGR ROUTINE: READ SEQUENTIAL ON DIRECTORY                                 
* SET CC NOT EQUAL IF ERROR OR END OF FILE                                      
**********************************************************************          
DSEQ     NTR1                                                                   
         L     R6,AWREC                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ ',=C'NTIDIR',KEY,(R6)                    
*                                                                               
         CLI   8(R1),0                                                          
         BNE   DSEQERR             ERROR OR EOF                                 
*                                                                               
         L     RE,AWREC                                                         
         LA    RE,4(RE)                                                         
         MVC   KEYFOUND,0(RE)                                                   
         B     DSEQOK                                                           
*                                                                               
DSEQOK   CR    RE,RE                                                            
         B     XIT                                                              
DSEQERR  CHI   RB,0                                                             
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CHECK INTSTA AGAINST LIST OF KNOWN STATION CODES.                             
* IF STATION CODE UNKNOWN, ADD BINARY STATION CODE TO TABLE UNKNTAB.            
* UNKNTAB WILL END UP CONTAINING 4-BYTE BINARY STATION CODES FOR THE            
* UKNOWN STATIONS.                                                              
* AT THE END OF THE CONVERSION, AN EMAIL WILL BE SENT WITH THE UNKNOWN          
* STATION CODES.                                                                
**********************************************************************          
KNOWNSTA NTR1                                                                   
*                                                                               
         OC    INTSTA,INTSTA                                                    
         BZ    KNSTAX                                                           
         TM    INTSTA,X'F0'        APPLIES ONLY TO NUMERIC                      
         BO    KNSTA05                                                          
         CLI   INTSTA,0            OR 3-BYTE BINARY STATION CODES               
         BE    KNSTA05                                                          
         B     KNSTAX                                                           
*                                                                               
KNSTA05  ICM   RF,15,ACOMFACS      CHECK TABLE OF REGULAR CABLE STATNS          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABNAM                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
                                                                                
         USING NECBNAMD,RE                                                      
KNSTA10  CLI   0(RE),X'FF'                                                      
         BE    KNSTA30             UNKNOWN STATION CODE                         
*                                                                               
         TM    INTSTA,X'F0'        STATION IS NUMERIC                           
         BNO   KNSTA20                                                          
         PACK  DUB,INTSTA(4)                                                    
         CVB   R5,DUB                                                           
         B     KNSTA25                                                          
*                                                                               
KNSTA20  ICM   R5,15,INTSTA        STATION IS BINARY                            
*                                                                               
KNSTA25  CLM   R5,7,NECBNNML                                                    
         BE    KNSTAX              THIS IS A KNOWN STATION                      
*                                                                               
         AR    RE,RF                                                            
         B     KNSTA10                                                          
         DROP  RE                                                               
*                                                                               
KNSTA30  ICM   RF,15,ACOMFACS      CHECK TABLE OF HISPANIC CABLE STATNS         
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEHCBNAM                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)         LENGTH OF TABLE ENTRY                        
                                                                                
         USING NEHCNAMD,RE                                                      
KNSTA35  CLI   0(RE),X'FF'                                                      
         BE    KNSTA50             UNKNOWN STATION CODE                         
*                                                                               
         TM    INTSTA,X'F0'        STATION IS NUMERIC                           
         BNO   KNSTA40                                                          
         PACK  DUB,INTSTA(4)                                                    
         B     KNSTA45                                                          
*                                                                               
KNSTA40  ICM   R0,15,INTSTA        STATION IS BINARY                            
         CVD   R0,DUB                                                           
*                                                                               
KNSTA45  MP    DUB,=P'10'                                                       
         CLC   NEHCNNML,DUB+4      STATION IS PACKED WITHOUT SIGN(PWOS)         
         BE    KNSTAX              THIS IS A KNOWN STATION                      
*                                                                               
         AR    RE,RF                                                            
         B     KNSTA35                                                          
         DROP  RE                                                               
*                                                                               
KNSTA50  LA    RE,UNKNTAB          ADD UNKNOWN STATION CODE TO TABLE            
KNSTA55  LA    RF,UNKNTAB                                                       
         LA    RF,UNKNTABL(RF)                                                  
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'                UNKNTAB TABLE FULL                           
         OC    0(4,RE),0(RE)                                                    
         BZ    KNSTA60                                                          
         CLM   R5,15,0(RE)                                                      
         BE    KNSTAX              STATION CODE ALREADY IN TABLE                
         LA    RE,4(RE)                                                         
         B     KNSTA55                                                          
*                                                                               
KNSTA60  STCM  R5,15,0(RE)         ADD STATION CODE TO TABLE                    
*                                                                               
KNSTAX   B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SEND AN EMAIL CONTAINING THE UNKNOWN STATION CODES.                           
* NOTE: THESE STATIONS ARE NOT LEFT OUT. THE DATA FOR THESE STATIONS            
*       WILL BE PART OF THE OUTPUT FILE.                                        
**********************************************************************          
*                                                                               
EMAILUNK NTR1                                                                   
*                                                                               
         OC    UNKNTAB(4),UNKNTAB                                               
         BZ    EMAILUNX                                                         
*                                                                               
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         LA    R1,ASIDFLD                                                       
         L     R5,0(R1)                                                         
         LOCASCB ASID=(R5)                                                      
         L     R5,ASCBASSB-ASCB(R1)                                             
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
*                                                                               
         L     R5,ASSBJSAB-ASSB(R5) R5 = A(JSAB)                                
         USING JSAB,R5                                                          
         L     RE,=A(SUBJJBID)                                                  
         MVC   0(8,RE),JSABJBID    SAVE JOBID/JOBNAME IN E-MAIL SUBJECT         
         L     RE,=A(SUBJJBNM)                                                  
         MVC   0(8,RE),JSABJBNM                                                 
         DROP  R5                                                               
*                                                                               
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',SENDTO),('SUBJCTLQ',SUBJECT)           
*                                                                               
         LA    R2,EMALIN1          ADD FIRST 3 LINES TO EMAIL BODY              
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R2,EMALIN2                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R2,EMALIN3                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
*                                                                               
         LA    R3,UNKNTAB                                                       
EMUNK10  LA    RF,UNKNTAB          ADD STATION CODES TO EMAIL BODY              
         LA    RF,UNKNTABL(RF)                                                  
         CR    R3,RF                                                            
         BNL   EMUNK50                                                          
         OC    0(4,R3),0(R3)                                                    
         BZ    EMUNK50                                                          
         MVC   EMALIN1,SPACES                                                   
         EDIT  (4,(R3)),(5,EMALIN1)                                             
         LA    R2,EMALIN1                                                       
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',(R2))                                  
         LA    R3,4(R3)                                                         
         B     EMUNK10                                                          
*                                                                               
EMUNK50  GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
EMAILUNX B     XIT                                                              
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------            
* LTORG, TABLES, AND CONSTANTS                                                  
*-------------------------------------------------------------------            
         LTORG                                                                  
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BFORMAT  DC    C'NTINPNN'                                                       
CFORMAT  DC    C'CABNCNN'                                                       
*                                                                               
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
CORRFLAG DC    AL1(NO)             CORRECTION FLAG                              
FRSTDDSB DC    AL1(YES)            FIRST TIME IN DDSNUM FOR BROADCAST           
*                                                                               
ASIDFLD  DC    F'0'                                                             
*                                                                               
MFILCNT  DC    AL4(0)              'M' FILE RECORD COUNT                        
MDIRCNT  DC    AL4(0)              'M' DIRECTORY RECORD COUNT                   
DPASCNT  DC    AL4(0)              'D' PASSIVE RECORD COUNT                     
TPASCNT  DC    AL4(0)              'T' PASSIVE RECORD COUNT                     
*                                                                               
DUPEXPSV DC    PL4'0'              # OF DUPLICATE EXTENDED PASSIVES             
PRVMEDIA DC    C' '                PREVIOUS MEDIA PROCESSED                     
PRVVTYPE DC    C' '                PREVIOUS VIEWING TYPE PROCESSED              
PREVMAJK DC    XL18'00'            PREVIOUS MAJOR KEY                           
*                                                                               
WARNMSG1 DS    0C                                                               
         DC    C'AUTONOTE*US-MFDEMOSPROGRAMMERS:'                               
WARNMS1# DC    C'NNNNNNN',C' DUPLICATE EXTENDED PASSIVE(S) SUPPRESSED.'         
         DC    C' SEE SYSPRIN2.'                                                
WARNMS1Q EQU   *-WARNMSG1                                                       
*                                                                               
SENDTO   DC    C'US-MFDEMOSPROGRAMMERS '                                        
*                                                                               
SUBJECT  DC    C'UNKNOWN STATIONS IN THE MXM CABLE DATA, '                      
SUBJJBID DS    CL8                                                              
         DC    C' '                                                             
SUBJJBNM DS    CL8                                                              
SUBJCTLQ EQU   *-SUBJECT                                                        
*                                                                               
EMALIN1  DC    CL80'YOU MAY NEED TO ADD THESE STATIONS TO DEMTABS.'             
EMALIN2  DC    CL80'DATA FOR STATIONS BELOW WAS LOADED.'                        
EMALIN3  DC    CL80'UNKNOWN CABLE STATION CODES:'                               
*                                                                               
*                                                                               
ALLMEDTB DC    AL1(IMEDBRO)        TABLE OF ALL MEDIAS TO DELETE                
         DC    AL1(IMEDCAB)        WHEN IDMEDIA=0                               
         DC    X'FF'                                                            
*                                                                               
ALLSRCTB DC    AL1(SRCLIVE)        TABLE OF ALL SOURCES TO DELETE               
         DC    AL1(SRCLIVSD)       FOR THE DIFFERENT VIEWING TYPES              
         DC    AL1(SRCLIVE7)                                                    
         DC    AL1(SRCLIVE1)                                                    
         DC    AL1(SRCLIVE2)                                                    
         DC    AL1(SRCLIVE3)                                                    
         DC    X'FF'                                                            
*                                                                               
NTIFILS  DS    0CL7                INDEXED TABLE OF NTIFIL NAMES                
         DC    CL7'NTIFILO'        01                                           
         DC    CL7'NTIFILN'        02                                           
         DC    CL7'NTIFILP'        03                                           
         DC    CL7'NTIFILQ'        04                                           
         DC    CL7'NTIFILW'        05                                           
         DC    CL7'NTIFILY'        06                                           
         DC    CL7'NTIFILZ'        07                                           
         DC    CL7'NTIFILA'        08                                           
         DC    CL7'NTIFILB'        09                                           
         DC    CL7'NTIFILC'        10                                           
         DC    CL7'NTIFILD'        11                                           
         DC    CL7'NTIFILE'        12                                           
         DC    CL7'NTIFILF'        13                                           
         DC    CL7'NTIFILG'        14                                           
         DC    CL7'NTIFILH'        15                                           
         DC    CL7'NTIFILI'        16                                           
         DC    CL7'NTIFILJ'        17                                           
         DC    CL7'NTIFILK'        18                                           
         DC    CL7'NTIFILL'        19                                           
         DC    CL7'NTIFILM'        20                                           
         DC    CL7'NTIFILR'        21                                           
         DC    CL7'NTIFL22'        22                                           
         DC    CL7'NTIFL23'        23                                           
         DC    CL7'NTIFL24'        24                                           
         DC    CL7'NTIFL25'        25                                           
         DC    CL7'NTIFL26'        26                                           
         DC    CL7'NTIFL27'        27                                           
         DC    CL7'NTIFL28'        28                                           
         DC    CL7'NTIFL29'        29                                           
         DC    CL7'NTIFL30'        30                                           
MAXFILES EQU   (*-NTIFILS)/L'NTIFILS   MAXIMUM # OF LOGICAL NTIFILS             
*                                                                               
NTITBL   DC    2500XL(NTITBLDL)'00'                                             
         DC    X'FFFFFFFFFF'                                                    
*                                                                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TEMP W/S                                                       
**********************************************************************          
*                                                                               
WORKD    DSECT                                                                  
THISKEY  DS    XL23                                                             
KEY      DS    XL23                                                             
KEYFOUND DS    XL23                                                             
PREVTKEY DS    XL18                PREVIOUS "T" MAJOR KEY WRITTEN               
*                                                                               
DELCNTS  DS    (MAXFILES)PL6       INDEXED # OF DELETED RECORDS BY FILE         
*                                                                               
CONDWORK DS    (CONWRKLN)X                                                      
CONWRKLN EQU   2000                                                             
*                                                                               
SVDA     DS    F                   SAVED DISK ADDRESS                           
SVSTATUS DS    C                   SAVED DIR STATUS FOR SPLIT FILE              
*                                                                               
COMMAND  DS    CL7                                                              
ELCODE   DS    X                                                                
DATADISP DS    H                                                                
*                                                                               
NTINUM   DS    HL2                 NTI (DDS) PROGRAM NUMBER                     
*                                                                               
SVVTYPE  DS    CL10                SAVED VIEWING TYPE DESCRIPTION               
*                                                                               
UNKNTAB  DS    10AL4               SAVED UNKNOWN CABLE CODES                    
UNKNTABL EQU   *-UNKNTAB                                                        
*                                                                               
DBUPARMS DS    XL(DBUPARML)        PARAMETER AREA FOR DEBKUPDT                  
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
NTITBLD  DSECT ,                   TABLE OF NIELSEN-TO-DDS PROG NUMBERS         
NTSTAT   DS    CL5                 STATION                                      
NTNIELSN DS    XL5                 NIELSEN PROGRAM NUMBER (PWOS)                
NTDDS    DS    HL2                 DDS (NTI) PROGRAM NUMBER                     
NTITBLDL EQU   *-NTITBLD                                                        
*                                                                               
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* INTERIM RECORD DSECT                                                          
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTMXMD                                                      
         EJECT                                                                  
       ++INCLUDE DEMXMIRD                                                       
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
* ++INCLUDE DEDEMFILE                                                           
* ++INCLUDE DDCOMFACS                                                           
* ++INCLUDE DEDEMCNVD                                                           
* ++INCLUDE DEDEMTABD                                                           
* ++INCLUDE DDMONYREQU                                                          
* ++INCLUDE DBUPARMSD                                                           
* ++INCLUDE DDSMTPD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
       ++INCLUDE DBUPARMSD                                                      
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
         PRINT ON                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DEMXMO    01/20/21'                                      
         END                                                                    
