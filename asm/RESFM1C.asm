*          DATA SET RESFM1C    AT LEVEL 236 AS OF 08/07/00                      
*PHASE T8181CA,*                                                                
         TITLE 'T8181C - RESFM1C - APPLICATION SECURITY LISTER'                 
*********************************************************************           
*                                                                   *           
*  RESFM1A (T8181C) --- APPLICATION SECURITY LISTER                 *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 01MAY00  (RHV)  INITIAL ENTRY                                     *           
*                                                                   *           
*********************************************************************           
*                                                                               
T8181C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**181C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         OI    GENSTAT5,GENSELVR                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PROCPFK        PF KEY HANDLER                               
         BE    PFK                                                              
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
* PF KEY HANDLER   PF12/24 - RETURN TO LIST FROM SELECT SCREEN *                
****************************************************************                
PFK      DS    0H                                                               
         CLI   PFAID,12                                                         
         BE    PF12                                                             
         CLI   PFAID,24                                                         
         BE    PF12                                                             
         B     XIT                                                              
*                                                                               
PF12     DS    0H                                                               
         CLI   ACTNUM,ACTSEL       SELECT ACTION NOW?                           
         BNE   XIT                 PF12 NOT APPROPRIATE                         
*                                                                               
***>     TM    SC3PFKH+1,X'0C'     ZERO INTENSITY PF KEY LABEL?                 
***?     BO    XIT                 YES - PF12 NOT APPROPRIATE                   
*                                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         OI    GENSTAT2,NEXTSEL                                                 
         B     XIT                                                              
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
VKEY     DS    0H                                                               
         BAS   RE,FACTLIST         RETRIEVE INFO FROM FACT                      
*                                                                               
         XC    KEY,KEY             READ REP RECORD                              
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVC   AIO,AIO1                                                         
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO,0,RFBLOCK                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
*                                                                               
         CLC   =X'0000',RREPMAST   MASTER/SUBSIDIARY?                           
         BE    VKEY0020            NO  - ACCEPT                                 
         CLC   =X'4040',RREPMAST   MASTER/SUBSIDIARY?                           
         BE    VKEY0020            NO  - ACCEPT                                 
         CLC   =X'FFFF',RREPMAST   YES - MASTER OR SUBSIDIARY?                  
         BE    VKEY0020            MASTER - ACCEPT                              
         MVC   RERROR,=AL2(806)    ERR - MUST BE MASTER                         
         LA    R2,CONACTH                                                       
         B     ERREND2                                                          
*                                                                               
VKEY0020 DS    0H                                                               
         XC    KEY,KEY                                                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    XIT                 DON'T BUILD LIST KEY HERE                    
*                                                                               
         LA    R6,KEY                                                           
         LA    R2,APMKF1H          PASS/PERS KEY FIELD                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKEY0030                                                         
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND              MISSING PASS/PERS                            
*                                                                               
VKEY0030 DS    0H                                                               
         MVC   ERROR,INVALID                                                    
         CLI   RECNUM,54            KEY ON PASSWORD?                            
         BNE   VKEY0040             NO                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         CLI   5(R2),L'SA0KCODE                                                 
         BH    ERREND                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SA0KCODE(0),8(R2)                                                
         OC    SA0KCODE,SPACES                                                  
         B     VKEY0050                                                         
         DROP  R6                                                               
*                                                                               
VKEY0040 DS    0H                                                               
         CLI   RECNUM,55            KEY ON PERSON?                              
         BE    *+6                                                              
         DC    H'0'                NO OTHER POSSIBILITIES                       
         USING SAPEREC,R6                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         MVC   SAPEPID,8(R2)                                                    
         OC    SAPEPID,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(SAPEDEF-SAPEKEY),KEYSAVE                                     
         BE    *+10                                                             
         MVC   KEY,KEYSAVE                                                      
         DROP  R6                                                               
*                                                                               
VKEY0050 DS    0H                                                               
         XC    FLTRPGM,FLTRPGM                                                  
         LA    R2,APMPGMH          PGM FILTER FIELD                             
         CLI   5(R2),0                                                          
         BE    VKEY0100                                                         
*                                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R3,RESDFPRG         PROGRAM TABLE (RESDFPRG)                     
VKEY0070 DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    ERREND              INVALID PROGRAM                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   3(R3),8(R2)         MATCH IN TABLE                               
         BE    VKEY0080            YES                                          
         LA    R3,L'RESDFPRG(R3)   NEXT PRG IN TABLE                            
         B     VKEY0070                                                         
VKEY0080 DS    0H                                                               
         MVC   FLTRPGM,0(R3)       USE PRG CODE TO FILTER                       
*                                                                               
VKEY0100 DS    0H                                                               
         MVC   AIO,AIO1            USE IO1 FOR CONTROL FILE RECORD              
         B     XIT                                                              
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
VREC     DS    0H                                                               
         MVC   CTLKEY,KEY          SAVE CTL FILE KEY                            
         L     R6,AIO                                                           
*                                                                               
* PASSWORD KEY                                                                  
*                                                                               
         CLI   RECNUM,54            KEY ON PASSWORD?                            
         BNE   VREC040              NO                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVC   APMKF1(L'SA0KCODE),SA0KCODE                                      
         DROP  R6                                                               
         MVI   ELCODE,SAPALELQ     PASSIVE POINTER ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM                          
         USING SAPALD,R6                                                        
         MVC   APMKF2(8),SAPALPID                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY FOR PERSON RECORD                  
         LA    R5,KEY                                                           
         USING SAPEREC,R5                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         MVC   SAPEPID,SAPALPID                                                 
         DROP  R6,R5                                                            
*                                                                               
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         CLC   KEY(SAPEDEF-SAPEKEY),KEYSAVE                                     
         BE    VREC050                                                          
         DC    H'0'                                                             
*                                                                               
* PERSON KEY                                                                    
*                                                                               
VREC040  DS    0H                                                               
         CLI   RECNUM,55            KEY ON PERSON?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SAPEREC,R6           BUILD PASSWORD KEY                          
*                                                                               
* FETCH PID NUM TO LINK TO REP SYSTEM RECORD                                    
*                                                                               
VREC050  DS    0H                                                               
         L     R6,AIO              CONTROL FILE RECORD                          
         MVI   ELCODE,SAPWDELQ     PASSWORD ELEM                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                WE REALLY NEED THIS                          
         USING SAPWDD,R6                                                        
         MVC   PIDNUM,SAPWDNUM                                                  
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY              READ PAR RECORD                              
         XC    KEY,KEY                                                          
         USING RSECREC,R6                                                       
         MVC   RSECKTYP(2),=X'1501'                                             
         MVC   RSECKREP,AGENCY                                                  
         MVC   RSECKPID,PIDNUM                                                  
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO2,0,RFBLOCK                       
         MVC   KEYSAVE2(4),8(R1)   D/A                                          
         BE    VREC100                                                          
*                                                                               
         L     R6,AIO2             NEED TO ADD RECORD                           
         XC    RSECKEY,RSECKEY                                                  
         MVC   RSECKTYP(2),=X'1501'                                             
         MVC   RSECKREP,AGENCY                                                  
         MVC   RSECKPID,PIDNUM                                                  
         XC    RSECDSEL(RSECSCEL-RSECDSEL+1),RSECDSEL                           
         LA    R1,RSECSCEL-RSECREC+1                                            
         STCM  R1,3,RSECLEN                                                     
         MVI   RSECDSCD,1                                                       
         MVI   RSECDSLN,RSECSCEL-RSECDSCD                                       
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFIL',KEYSAVE2,AIO2,DMWORK          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC100  DS    0H                                                               
         L     R6,AIO2             PAR RECORD                                   
         LA    R3,APMPGM1H         1ST LINE (PROGRAM FIELD)                     
         LA    R2,APMACC1H         1ST LINE (ACCESS FIELD)                      
*                                                                               
VREC200  DS    0H                                                               
         LA    RF,APMPGMXH                                                      
         CR    R3,RF                                                            
         BH    VREC500             DONE WITH SCREEN - UPD RECORD                
*                                                                               
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    VREC400             NO - NEXT LINE                               
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSDFREC,R5                                                       
         MVC   RSDFKTYP(2),=X'1502'                                             
         MVC   RSDFKREP,AGENCY                                                  
         MVC   RSDFKPRG,8(R3)                                                   
         OC    RSDFKPRG,SPACES                                                  
         ZIC   RF,0(R3)                                                         
         AR    RF,R3               REC FIELD                                    
         MVC   RSDFKREC,8(RF)                                                   
         OC    RSDFKREC,SPACES                                                  
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO3,0,RFBLOCK                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  DELETE EXITING PRG SEC ELEMS                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'70',RSECREC),           +        
               (11,RSDFKPRG)                                                    
*                                                                               
         XC    ELEM,ELEM           BUILD NEW PAR PRG SEC ELEM                   
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,RSECPGLQ                                                  
         MVC   ELEM+2(11),RSDFKPRG                                              
*                                                                               
         CLI   8(R2),C'='          INVOKE BROWSE?                               
         BNE   VREC230             NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK+4(11),RSDFKPRG                                              
         GOTO1 (RFBROWSE,REPFACS),DMCB,ACOMFACS,BASERD,(R2),0,         +        
               (0,C' ACC'),(0,WORK)                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VREC230  DS    0H                                                               
         ZIC   R1,5(R2)            CHECK "NOACCESS"                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL8'NOACCESS'                                           
         BNE   VREC250                                                          
         MVC   ELEM+13(8),=CL8'NOACCESS'                                        
         B     VREC350                                                          
*                                                                               
VREC250  DS    0H                  CHECK "ALL"                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL8'ALL'                                                
         BNE   VREC280                                                          
         MVC   ELEM+13(8),=CL8'ALL'                                             
         B     VREC350                                                          
*                                                                               
VREC280  DS    0H                  CHECK "DEFAULT"                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL8'DEFAULT'                                            
         BE    VREC400                                                          
*                                                                               
VREC300  DS    0H                                                               
         L     R5,AIO3             SECDEF REC                                   
         USING RSDFREC,R5                                                       
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',RSDFREC),           +        
               (5(R2),8(R2))                                                    
         MVI   ERROR,INVALID                                                    
         CLI   12(R1),0            GOT IT?                                      
         BNE   ERREND              NO - INVALID LEVEL NAME                      
         L     R1,12(R1)           ELEM                                         
         MVC   ELEM+13(8),2(R1)    FULL LEVEL NAME TO NEW ELEM                  
*                                                                               
VREC350  DS    0H                  ADD ELEMENT                                  
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),RSECREC,ELEM,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC400  DS    0H                  NEXT SCREEN LINE                             
         LA    R3,APMPGM2-APMPGM1(R3)                                           
         LA    R2,APMACC2-APMACC1(R2)                                           
         B     VREC200                                                          
*                                                                               
VREC500  DS    0H                  WRITE RECORD TO FILE                         
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFIL',KEYSAVE2,    +        
               AIO3,DMWORK                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFIL',KEY,AIO2,DMWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1             CONTROL REC                                  
         MVC   KEY,CTLKEY                                                       
         GOTO1 READ                RESTORE CONTROL REC                          
         DROP  R5,R6                                                            
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
DREC     DS    0H                                                               
         L     R6,AIO              CONTROL FILE RECORD                          
*                                                                               
         MVC   KEYSAVE2,KEY        TO RESTORE LATER                             
*                                                                               
         XC    APMKF1,APMKF1       CLEAR KEY FIELDS ON SCREEN                   
         OI    APMKF1H+6,X'80'                                                  
         XC    APMKF2,APMKF2                                                    
         OI    APMKF2H+6,X'80'                                                  
*                                                                               
         LA    R2,APMPGM1H                                                      
         LA    RF,APMDFTXH                                                      
DREC020  DS    0H                                                               
         CR    R2,RF                                                            
         BH    DREC030                                                          
         ZIC   R1,0(R2)                                                         
         AHI   R1,-9                                                            
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+4                                                           
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DREC020                                                          
*                                                                               
* PASSWORD KEY                                                                  
*                                                                               
DREC030  DS    0H                                                               
         CLI   RECNUM,54            KEY ON PASSWORD?                            
         BNE   DREC040              NO                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVC   APMKF1(L'SA0KCODE),SA0KCODE                                      
         DROP  R6                                                               
         MVI   ELCODE,SAPALELQ     PASSIVE POINTER ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM                          
         USING SAPALD,R6                                                        
         MVC   APMKF2(8),SAPALPID                                               
*                                                                               
         XC    KEY,KEY             BUILD KEY FOR PERSON RECORD                  
         LA    R5,KEY                                                           
         USING SAPEREC,R5                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         MVC   SAPEPID,SAPALPID                                                 
         DROP  R6,R5                                                            
*                                                                               
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         CLC   KEY(SAPEDEF-SAPEKEY),KEYSAVE                                     
         BE    DREC050                                                          
         DC    H'0'                                                             
*                                                                               
* PERSON KEY                                                                    
*                                                                               
DREC040  DS    0H                                                               
         CLI   RECNUM,55            KEY ON PERSON?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SAPEREC,R6           BUILD PASSWORD KEY                          
         MVC   APMKF1(L'SAPEPID),SAPEPID                                        
         DROP  R6                                                               
         MVI   ELCODE,SAPWDELQ     PERSON/PASSWORD ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM TO BE THERE              
         USING SAPWDEL,R6                                                       
         MVC   APMKF2(L'SAPWDCOD),SAPWDCOD                                      
         DROP  R6                                                               
*                                                                               
DREC050  DS    0H                                                               
         XC    APMFNM,APMFNM       CLEAR 1ST NAME FIELD                         
         OI    APMFNMH+6,X'80'     XMIT                                         
         XC    APMLNM,APMLNM       CLEAR 1ST NAME FIELD                         
         OI    APMLNMH+6,X'80'     XMIT                                         
         L     R6,AIO                                                           
         MVI   ELCODE,SANAMELQ     PERSON NAME ELEM                             
         BAS   RE,GETEL                                                         
         BNE   DREC070                                                          
         USING SANAMD,R6                                                        
         LA    R4,SANAMES          SET A(1ST LENGTH FIELD)                      
         TM    SANAMIND,X'80'      FIRST NAME PRESENT?                          
         BNO   DREC080             NO                                           
         ZIC   R1,0(R4)            L'FIRST NAME                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APMFNM(0),1(R4)                                                  
         LA    R4,2(R1,R4)         NEXT NAME SEGMENT                            
DREC060  DS    0H                                                               
         TM    SANAMIND,X'40'      MIDDLE NAME PRESENT?                         
         BNO   DREC070             NO                                           
         ZIC   RF,0(R4)            BUMP TO NEXT NAME SEGMENT                    
         LA    RF,1(RF)            ADD 1 FOR L(CONTROL BYTE)                    
         AR    R4,RF                                                            
DREC070  DS    0H                                                               
         TM    SANAMIND,X'20'      LAST NAME PRESENT?                           
         BNO   DREC080             NO                                           
         ZIC   R1,0(R4)            L'LAST NAME                                  
         CLI   0(R4),L'APMLNM                                                   
         BNH   *+8                                                              
         LA    R1,L'APMLNM                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APMLNM(0),1(R4)                                                  
         DROP  R6                                                               
*                                                                               
DREC080  DS    0H                                                               
         XC    APMOFC,APMOFC                                                    
         OI    APMOFCH+6,X'80'                                                  
         XC    APMDPT,APMDPT                                                    
         OI    APMDPTH+6,X'80'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,SAPERELQ     PERSONNEL DETAILS ELEM                       
         BAS   RE,GETEL                                                         
         BNE   DREC090                                                          
         USING SAPERD,R6                                                        
         MVC   APMOFC,SAPEROFF     OFFICE                                       
         MVC   APMDPT,SAPERDID     DEPT                                         
         DROP  R6                                                               
*                                                                               
* FETCH PID NUM TO READ OTHER RECORDS                                           
*                                                                               
DREC090  DS    0H                                                               
         L     R6,AIO              CONTROL FILE RECORD                          
         MVI   ELCODE,SAPWDELQ     PASSWORD ELEM                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                WE REALLY NEED THIS                          
         USING SAPWDD,R6                                                        
         MVC   PIDNUM,SAPWDNUM                                                  
         DROP  R6                                                               
*                                                                               
* LOOKUP USER ID'S                                                              
*                                                                               
         XC    KEY,KEY             FETCH PERSONAL AUTH/PASSWORD REC             
         LA    R6,KEY                                                           
         USING SA0REC,R6                                                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVC   SA0KNUM,PIDNUM                                                   
         DROP  R6                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         CLC   KEY(L'SA0KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE                                      
         L     R6,AIO2                                                          
*                                                                               
* READ REP SECURITY RECORD                                                      
*                                                                               
DREC140  DS    0H                                                               
         LA    R6,KEY              READ PAR RECORD                              
         XC    KEY,KEY                                                          
         USING RSECREC,R6                                                       
         MVC   RSECKTYP(2),=X'1501'                                             
         MVC   RSECKREP,AGENCY                                                  
         MVC   RSECKPID,PIDNUM                                                  
         L     R6,AIO2                                                          
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO2,0,RFBLOCK                       
         BE    *+8                                                              
         MVI   0(R6),0             NO RECORD INDICATOR                          
*                                                                               
         LA    R2,APMPGM1H         FIRST DISPL LINE HEADER                      
*                                                                               
         LA    R5,KEY              READ SECDEF RECORDS                          
         XC    KEY,KEY                                                          
         USING RSDFREC,R5                                                       
         MVC   RSDFKTYP(2),=X'1502'                                             
         MVC   RSDFKREP,AGENCY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         B     DREC210                                                          
*                                                                               
DREC200  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEY,KEY,0                     
         LA    R5,KEY                                                           
DREC210  DS    0H                                                               
         CLC   KEY(15),KEYSAVE                                                  
         BNE   DRECX                                                            
*                                                                               
         LA    RF,APMPGMXH         LAST DISPL LINE                              
         CR    R2,RF                                                            
         BH    DRECX               NO MORE ROOM                                 
*                                                                               
         LA    R3,RESDFPRG         PGM NAME TABLE                               
DREC220  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RSDFKPRG,0(R3)                                                   
         BE    *+12                                                             
         LA    R3,L'RESDFPRG(R3)                                                
         B     DREC220                                                          
         MVC   8(8,R2),3(R3)       PGM EXPANSION TO SCREEN                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               RECORD FIELD                                 
*                                                                               
         MVC   8(L'APMREC1,R2),RSDFKREC   REC NAME TO SCREEN                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               ACCESS FIELD                                 
*                                                                               
         CLI   RSECREC,0           NO PAR REC, USE DEFAULT                      
         BE    DREC300                                                          
*                                                                               
DREC240  DS    0H                  CHECK FOR MATCHING PAR ELEMENT               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'70',RSECREC),           +        
               (11,RSDFKPRG)                                                    
         CLI   12(R1),0            GOT IT                                       
         BNE   DREC300                                                          
         L     R1,12(R1)           ELEM                                         
         MVC   8(8,R2),13(R1)      SECURITY LEVEL                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               DEFAULT FIELD                                
         XC    8(8,R2),8(R2)       CLEAR                                        
         B     DREC400                                                          
*                                                                               
DREC300  DS    0H                  NO MATCHING PAR ELEM, USE DEFAULT            
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO3,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3                                                          
         MVC   8(7,R2),=C'DEFAULT'                                              
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               DEFAULT FIELD                                
         MVC   8(8,R2),RSDFIDF                                                  
*                                                                               
DREC400  DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE                                    
         B     DREC200             NEXT SECDEF RECORD                           
*                                                                               
DRECX    DS    0H                                                               
         MVC   KEY,KEYSAVE2        RESTORE REC WE CAME IN WITH                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R5,R6                                                            
****************************************************************                
*                        LIST ROUTINE                          *                
****************************************************************                
LIST     DS    0H                                                               
         LA    R6,KEY                                                           
         OC    KEY,KEY             INITIALIZED                                  
         BZ    LIST010             NO                                           
*                                                                               
         CLI   RECNUM,54           CHECK IF REC CHANGED & WE NEED TO            
         BNE   *+12                RE INIT LIST KEY                             
         CLI   KEY,SA0KTYPQ                                                     
         BNE   LIST010                                                          
         CLI   RECNUM,55                                                        
         BNE   LIST030                                                          
         CLI   KEY,SAPETYPQ                                                     
         BE    LIST030                                                          
*                                                                               
LIST010  DS    0H                  BUILD LIST KEY                               
         XC    KEY,KEY                                                          
*                                                                               
         CLI   RECNUM,54           KEY ON PASSWORD?                             
         BNE   LIST015                                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVI   SA0KCODE,1           FOR LIST, START AT 1ST PASSWORD             
         CLI   APLKF1H+5,0         ANY START AT?                                
         BE    LIST020             NO                                           
         MVC   SA0KCODE(L'APLKF1),APLKF1                                        
         B     LIST020                                                          
         DROP  R6                                                               
*                                                                               
LIST015  DS    0H                                                               
         CLI   RECNUM,55           KEY ON PERSON? ?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SAPEREC,R6                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         CLI   APLKF1H+5,0         ANY START AT?                                
         BE    LIST020             NO                                           
         MVC   SAPEPID,APLKF1                                                   
         DROP  R6                                                               
*                                                                               
LIST020  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         B     LIST040                                                          
*                                                                               
LIST030  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
LIST040  DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
*                                                                               
         CLI   RECNUM,54           KEY ON PASSWORD?                             
         BNE   LIST100             NO - TRY PERSON                              
         USING SA0REC,R6                                                        
         CLC   KEY(SA0KCODE-SA0KEY),KEYSAVE                                     
         BNE   XIT                                                              
         CLI   SA0KCODE,0          END OF THIS RECORD FLAVOR?                   
         BE    XIT                 YEP, DONE                                    
*                                                                               
         MVC   LPASWRD2,SA0KCODE   PASSWORD                                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'C3'        PERSONAL ID ELEM                             
         BAS   RE,GETEL                                                         
         BNE   LIST030             BAD RECORD - CAN'T WORK WITH IT              
         USING SAPALD,R6                                                        
         MVC   LPERID2,SAPALPID                                                 
*                                                                               
         MVC   KEYSAVE2,KEY        SO WE CAN RSTORE THIS REC                    
*                                                                               
         XC    KEY,KEY             BUILD KEY FOR PERSON RECORD                  
         LA    R5,KEY                                                           
         USING SAPEREC,R5                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         MVC   SAPEPID,SAPALPID                                                 
         DROP  R6,R5                                                            
*                                                                               
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         CLC   0(SAPEDEF-SAPEKEY,R6),KEY                                        
         BE    LIST200                                                          
         DC    H'0'                                                             
*                                                                               
LIST100  DS    0H                                                               
         CLI   RECNUM,55           KEY ON PERSON?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(SAPEPID-SAPEKEY),KEYSAVE                                     
         BNE   XIT                                                              
*                                                                               
         CLC   KEY(SAPEDEF-SAPEKEY),KEYSAVE                                     
         BE    LIST030             SAME PID                                     
*                                                                               
         MVC   KEYSAVE2,KEY        SO WE CAN RSTORE THIS REC                    
*                                                                               
         USING SAPEREC,R6                                                       
         MVC   LPERID,SAPEPID      PERSON                                       
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              PERSONAL ID ELEMENT                          
         MVI   ELCODE,X'C4'                                                     
         BAS   RE,GETEL                                                         
         BNE   LIST200                                                          
         USING SAPWDD,R6                                                        
         MVC   LPASWRD,SAPWDCOD                                                 
         DROP  R6                                                               
*                                                                               
LIST200  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'C6'        PERSON DETAIL ELEM                           
         BAS   RE,GETEL                                                         
         BNE   LIST205                                                          
         USING SAPEREL,R6                                                       
         OC    SAPERDTE,SAPERDTE   TERMINATION DATE?                            
         BNZ   LIST350             YES EXCLUDE FROM LIST                        
         DROP  R6                                                               
*                                                                               
LIST205  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,SANAMELQ     PERSON NAME ELEM                             
         BAS   RE,GETEL                                                         
         BNE   LIST230                                                          
         USING SANAMD,R6                                                        
         LA    R4,SANAMES          SET A(1ST LENGTH FIELD)                      
         TM    SANAMIND,X'80'      FIRST NAME PRESENT?                          
         BNO   LIST210             NO                                           
         MVC   LPERNAM(1),1(R4)    YES - TAKE 1ST CHARACTER                     
         ZIC   RF,0(R4)            BUMP TO NEXT NAME SEGMENT                    
         LA    RF,1(RF)            ADD 1 FOR L(CONTROL BYTE)                    
         AR    R4,RF                                                            
LIST210  DS    0H                                                               
         TM    SANAMIND,X'40'      MIDDLE NAME PRESENT?                         
         BNO   LIST220             NO                                           
         MVC   LPERNAM+2(1),1(R4)  YES - TAKE 1ST CHARACTER                     
         ZIC   RF,0(R4)            BUMP TO NEXT NAME SEGMENT                    
         LA    RF,1(RF)            ADD 1 FOR L(CONTROL BYTE)                    
         AR    R4,RF                                                            
LIST220  DS    0H                                                               
         TM    SANAMIND,X'20'      LAST NAME PRESENT?                           
         BNO   LIST230             NO                                           
         ZIC   RF,0(R4)            YES - TAKE ENTIRE LAST NAME                  
         BCTR  RF,0                BACK OFF 1 CHAR                              
         CH    RF,=H'11'                                                        
         BNH   *+8                                                              
         LA    RF,11                                                            
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   LPERNAM+4(0),1(R4)  MOVE BY LENGTH                               
         DROP  R6                                                               
*                                                                               
LIST230  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,SAPERELQ     PERSONNEL DETAILS ELEM                       
         BAS   RE,GETEL                                                         
         BNE   LIST250                                                          
         USING SAPERD,R6                                                        
         MVC   LOFF,SAPEROFF       INSERT OFFICE                                
         MVC   LDPT,SAPERDID       INSERT DEPARTMENT                            
         DROP  R6                                                               
LIST250  DS    0H                                                               
         CLI   APLOFCH+5,0         OFFICE FILTER?                               
         BE    LIST260             NO                                           
         CLC   LOFF,APLOFC         MATCH FILTER?                                
         BNE   LIST350             NO                                           
LIST260  DS    0H                                                               
         CLI   APLDPTH+5,0         DEPT FILTER?                                 
         BE    LIST270             NO                                           
         CLC   LDPT,APLDPT         MATCH FILTER?                                
         BNE   LIST350             NO                                           
*                                                                               
LIST270  DS    0H                                                               
         L     R6,AIO              CONTROL FILE RECORD                          
         MVI   ELCODE,SAPWDELQ     PASSWORD ELEM                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                WE REALLY NEED THIS                          
         USING SAPWDD,R6                                                        
         MVC   PIDNUM,SAPWDNUM                                                  
         DROP  R6                                                               
*                                                                               
LIST300  DS    0H                                                               
***>     MVC   KEYSAVE,KEY                                                      
         L     R6,AIO                                                           
         MVC   KEY,KEYSAVE2        RESTORE PASS/PERS REC KEY                    
         GOTO1 LISTMON             INSERT INTO LIST                             
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLC   KEY(L'SA0KEY),KEYSAVE                                            
         BE    LIST030             NEXT RECORD                                  
         DC    H'0'                FAILED TO RESTORE PASS REC                   
*                                                                               
LIST350  DS    0H                                                               
***>     MVC   KEYSAVE,KEY                                                      
         L     R6,AIO                                                           
         MVC   KEY,KEYSAVE2        RESTORE PASS/PERS REC KEY                    
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLC   KEY(L'SA0KEY),KEYSAVE                                            
         BE    LIST030             NEXT RECORD                                  
         DC    H'0'                FAILED TO RESTORE PASS REC                   
***********************************************************************         
*   DISPF12 - DISPLAY PF12 LABEL WHEN APPROPRIATE                               
***********************************************************************         
DISPF12  NTR1                                                                   
         LA    R2,APMPFKH                                                       
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DPF12020                                                         
         NI    1(R2),X'FF'-X'04'  HIGH INTENSITY                                
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
DPF12020 DS    0H                                                               
         OI    1(R2),X'04'     LOW INTENSITY                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
*   FACTLIST:  ACCESS GETFACT, RETRIEVE PERTINENT INFORMATION                   
***********************************************************************         
FACTLIST NTR1                                                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   SECAGY,FATAGYSC     AGY CODE FOR SECURITY                        
         DROP  RF                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     EQU   *                                                                
XIT      XIT1                                                                   
*                                                                               
ERREND   EQU   *                                                                
         GOTO1 ERREX               ERROR NUM IN 'ERROR'  (1 BYTE)               
ERREND2  EQU   *                                                                
         GOTO1 MYERROR             ERROR NUM IN 'RERROR' (2 BYTE)               
         SPACE 3                                                                
RELO     DS    A                                                                
*                                                                               
         LTORG                                                                  
       ++INCLUDE RESDFPRG                                                       
         EJECT                                                                  
LISTD    DSECT                                                                  
LPERID   DS    CL8                 PERSONAL ID SCREEN DISPLAY                   
         DS    CL1                                                              
LPASWRD  DS    CL10                                                             
         DS    CL1                                                              
         ORG   LPERID                                                           
LPASWRD2 DS    CL10                PASSWORD SCREEN DISPLAY                      
         DS    CL1                                                              
LPERID2  DS    CL8                                                              
         DS    CL1                                                              
*                                                                               
LPERNAM  DS    CL16                                                             
         DS    CL1                                                              
LOFF     DS    CL2                                                              
         DS    CL1                                                              
LDPT     DS    CL3                                                              
         DS    CL41                                                             
*                                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMWORKD                                                                    
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM94D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM92D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
KEYSAVE2 DS    CL27                                                             
CTLKEY   DS    CL(L'KEY)                                                        
*                                                                               
FLTRPGM  DS    CL3                 PROGRAM FILTER                               
*                                                                               
SUBSCRN  DS    X                   SUBSCREEN TYPE                               
SCLINES  DS    X                   NUMBER OF SCANNER LINES                      
*                                                                               
SECAGY   DS    CL2                 AGENCY FOR SECURITY RECORDS                  
PIDNUM   DS    CL2                                                              
*                                                                               
         DSECT                                                                  
       ++INCLUDE REGENSEC                                                       
         DSECT                                                                  
       ++INCLUDE REGENSDF                                                       
         DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'236RESFM1C   08/07/00'                                      
         END                                                                    
