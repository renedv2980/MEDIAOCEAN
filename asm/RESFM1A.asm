*          DATA SET RESFM1A    AT LEVEL 202 AS OF 04/25/01                      
*PHASE T8181AA,*                                                                
         TITLE 'T8181A - RESFM1A - SECURITY LISTER'                             
*********************************************************************           
*                                                                   *           
*  RESFM1A (T8181A) --- SECURITY LISTER                             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 10JUN98  (BU )  INITIAL ENTRY                                     *           
*                                                                   *           
* 02FEB99  (RHV)  APPLICATON SECURITY                               *           
*                                                                   *           
* 25JAN00  (RHV)  SALESPERSON SECURITY                              *           
*                                                                   *           
* 05APR01  (RHV)  TEAMS AS SETS OF S/P                              *           
*                                                                   *           
*********************************************************************           
*                                                                               
T8181A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**181A**,R7,RR=R3                                              
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
         LA    R2,SC3KF1H          PASS/PERS KEY FIELD                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKEY0030                                                         
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND              MISSING PASS/PERS                            
*                                                                               
VKEY0030 DS    0H                                                               
         MVC   ERROR,INVALID                                                    
         CLI   RECNUM,47            KEY ON PASSWORD?                            
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
         CLI   RECNUM,48            KEY ON PERSON?                              
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
         MVC   AIO,AIO1            USE IO1 FOR CONTROL FILE RECORD              
*                                                                               
         BAS   RE,VSUB                                                          
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
VREC     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVC   BYTE,SUBSCRN        SAVE FOR COMPARISON                          
         BAS   RE,VSUB             CHECK FOR SUBSCREEN SELECTOR CHANGE          
*                                                                               
* PASSWORD KEY                                                                  
*                                                                               
         CLI   RECNUM,47            KEY ON PASSWORD?                            
         BNE   VREC040              NO                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVC   SC3KF1(L'SA0KCODE),SA0KCODE                                      
         DROP  R6                                                               
         MVI   ELCODE,SAPALELQ     PASSIVE POINTER ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM                          
         USING SAPALD,R6                                                        
         MVC   SC3KF2(8),SAPALPID                                               
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
         CLI   RECNUM,48            KEY ON PERSON?                              
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
         LA    R6,KEY              READ REP SEC RECORD                          
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
         DROP  R6                                                               
*                                                                               
VREC100  DS    0H                                                               
         CLC   BYTE,SUBSCRN        CHANGE OF SUBSCREEN?                         
         BE    VREC110             NO CHANGE                                    
         L     R6,AIO2             REPSEC RECORD                                
         BAS   RE,DISCODES         DISPLAY SUBSCREEN                            
         B     XIT                 DON'T UPDATE ON THIS PASS                    
*                                                                               
* SCREEN FIELDS TO VALIDATE                                                     
*   A  A(FIELD HEADER)                                                          
*   X  SUB RECORD CODE IN ELEMENT                                               
FLDSUBS  DS    0CL5                SUBSIDIARY FIELDS                            
         DC    AL4(SC3VA1H-SCRNTOP),X'01' (MUST BE FIRST)                       
         DC    AL4(SC3NC1H-SCRNTOP),X'02'                                       
         DC    AL4(SC3NS1H-SCRNTOP),X'03'                                       
         DC    AL4(SC3NM1H-SCRNTOP),X'04'                                       
         DC    AL4(SC3NO1H-SCRNTOP),X'05'                                       
         DC    AL4(SC3NW1H-SCRNTOP),X'06'                                       
         DC    AL4(SC3NL1H-SCRNTOP),X'07'                                       
         DC    4X'00'                                                           
FLDMAST  DS    0CL5                MASTER FIELDS                                
         DC    AL4(SC3VA2H-SCRNTOP),X'01' (MUST BE FIRST)                       
         DC    AL4(SC3NC2H-SCRNTOP),X'02'                                       
         DC    AL4(SC3NS2H-SCRNTOP),X'03'                                       
         DC    AL4(SC3NM2H-SCRNTOP),X'04'                                       
         DC    AL4(SC3NO2H-SCRNTOP),X'05'                                       
         DC    AL4(SC3NW2H-SCRNTOP),X'06'                                       
         DC    AL4(SC3NL2H-SCRNTOP),X'07'                                       
         DC    4X'00'                                                           
SCRNTOP  EQU   CONHEADH-64                                                      
*                                                                               
VREC110  DS    0H                  DELETE OLD ELEMENTS                          
         MVC   ELCODE,SUBSCRN      ELEM NUM FROM SUBSCREEN CODE                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(ELCODE,AIO2),0,0                   
         TM    12(R1),X'FF'-X'06'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,ELCODE                                                      
         OI    ELCODE,X'01'                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(ELCODE,AIO2),0,0                   
         TM    12(R1),X'FF'-X'06'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   ELCODE,BYTE                                                      
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R3,FLDSUBS          SUBSIDIARY FIELDS                            
VREC200  DS    0H                                                               
         ZICM  R2,0(R3),4          SCREEN FIELD DISPLACEMENT                    
         BZ    VREC300             DONE                                         
         AR    R2,RA               FIELD ADDRESS                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VREC290             NO                                           
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VREC210                                                          
         CLC   =C'ALL',8(R2)                                                    
         BNE   VREC210                                                          
         CLI   4(R3),1             VALID ACCESS (S1) FIELD?                     
         BE    VREC290             DON'T WRITE ELEM                             
*                                                                               
VREC210  DS    0H                                                               
         XC    ELEM,ELEM           PREPARE EMPTY ELEM                           
         LA    R6,ELEM                                                          
         USING RSECSCEL,R6                                                      
         MVC   RSECSCCD,ELCODE     ELEM CODE                                    
         MVI   RSECSCLN,3          EMPTY ELEM LEN                               
         MVC   RSECSCSB,4(R3)      ELEM SUB CODE FOR THIS FIELD                 
         DROP  R6                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO3                                           
         MVC   SCLINES,4(R1)       SAVE # OF LINES                              
         ZICM  R5,4(R1),1          # OF LINES                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3             SCANNER OUTPUT                               
         B     *+8                                                              
VREC220  DS    0H                                                               
         LA    R4,32(R4)           NEXT SCANNER LINE                            
         CLI   1(R4),0             2ND SCANNER FIELD                            
         BNE   ERREND              SHOULDN'T HAVE 2ND FIELD                     
*                                                                               
         BAS   RE,VALCODE          VALIDATE RECORD CODE & PUT IN ELEM           
         BNL   VREC250             CODE NOT AN EXCLUSION SET                    
         CLI   SCLINES,1           MORE THAN ONE CODE INPUT?                    
         BNH   VREC250             NO - OK                                      
         MVC   RERROR,=AL2(847)    YES - NOT VALID W/EXCLUSION SETS             
         B     ERREND2                                                          
VREC250  DS    0H                                                               
         BCT   R5,VREC220          NEXT SCANNER LINE                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),AIO2,ELEM,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC290  DS    0H                                                               
         LA    R3,L'FLDSUBS(R3)    NEXT FIELD IN TABLE                          
         B     VREC200                                                          
*                                                                               
VREC300  DS    0H                  MASTER FIELDS                                
         MVI   ERROR,INVALID                                                    
         LA    R3,FLDMAST          MASTER FIELDS                                
         OI    ELCODE,X'01'        MAKE MASTER ELEM CODE                        
VREC305  DS    0H                                                               
         ZICM  R2,0(R3),4          SCREEN FIELD DISPLACEMENT                    
         BZ    VREC500             DONE                                         
         AR    R2,RA               FIELD ADDRESS                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VREC390             NO                                           
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VREC310                                                          
         CLC   =C'ALL',8(R2)                                                    
         BNE   VREC310                                                          
         CLI   4(R3),1             VALID ACCESS (S1) FIELD?                     
         BE    VREC390             DON'T WRITE ELEM                             
*                                                                               
VREC310  DS    0H                                                               
         XC    ELEM,ELEM           PREPARE EMPTY ELEM                           
         LA    R6,ELEM                                                          
         USING RSECSCEL,R6                                                      
         MVC   RSECSCCD,ELCODE     ELEM CODE                                    
         MVI   RSECSCLN,3          EMPTY ELEM LEN                               
         MVC   RSECSCSB,4(R3)      ELEM SUB CODE FOR THIS FIELD                 
         DROP  R6                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO3                                           
         MVC   SCLINES,4(R1)       SAVE # OF LINES                              
         ZICM  R5,4(R1),1          # OF LINES                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3             SCANNER OUTPUT                               
         B     *+8                                                              
VREC320  DS    0H                                                               
         LA    R4,32(R4)           NEXT SCANNER LINE                            
         CLI   1(R4),0             2ND SCANNER FIELD                            
         BNE   ERREND              SHOULDN'T HAVE 2ND FIELD                     
*                                                                               
         BAS   RE,VALCODE          VALIDATE RECORD CODE & PUT IN ELEM           
         BNL   VREC350             CODE NOT AN EXCLUSION SET                    
         CLI   SCLINES,1           MORE THAN ONE CODE INPUT?                    
         BNH   VREC350             NO - OK                                      
         MVC   RERROR,=AL2(847)    YES - NOT VALID W/EXCLUSION SETS             
         B     ERREND2                                                          
VREC350  DS    0H                                                               
         BCT   R5,VREC320          NEXT SCANNER LINE                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),AIO2,ELEM,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC390  DS    0H                                                               
         LA    R3,L'FLDMAST(R3)    NEXT FIELD IN TABLE                          
         B     VREC305                                                          
*                                                                               
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
         L     R6,AIO2                                                          
         BAS   RE,DISCODES         DISPLAY SUBSCREEN                            
         B     XIT                 DON'T UPDATE ON THIS PASS                    
*                                                                               
* VALIDATE CODE & PUT CODE IN NEW ELEMENT                                       
* PRECONDITIONS:                                                                
*   R4: ADDRESSES CURRENT SCANNER LINE                                          
*   ELEM: CONTAINS PARTIALLY CONSTRUCTED NEW ELEMENT                            
*                                                                               
VALCODE  NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         CLI   12(R4),C'*'         SET?                                         
         BE    VCOD070             YES - DEAL WITH THIS SEPARATELY              
*                                                                               
         CLI   SUBSCRN,X'60'       SALESPERSON?                                 
         BNE   *+12                                                             
         CLI   12(R4),C'#'         TEAM?                                        
         BE    VCOD065             YES - DEAL WITH THIS SEPARATELY              
*                                                                               
         CLI   SUBSCRN,X'10'       GROUP                                        
         BNE   VCOD020                                                          
         CLI   0(R4),2                                                          
         BH    ERREND              CODE TOO LONG                                
         USING RGRPREC,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,12(R4)                                                  
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD020  DS    0H                                                               
         CLI   SUBSCRN,X'20'       OFFICE                                       
         BNE   VCOD030                                                          
         CLI   0(R4),2                                                          
         BH    ERREND              CODE TOO LONG                                
         USING ROFFREC,R6                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,12(R4)                                                  
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD030  DS    0H                                                               
         CLI   SUBSCRN,X'30'       OWNER                                        
         BNE   VCOD040                                                          
         ZIC   R1,0(R4)            CODE LEN                                     
         CLI   ELEM+2,X'01'        ARE WE DEALING WITH OWNER, S1?               
         BNE   VCOD036             NO - PROCEED NORMALLY                        
         LA    RF,11(R4,R1)        LAST CHARACTER OF CODE                       
         CLI   0(RF),C'+'          SPECIAL CASE?                                
         BNE   VCOD036             NO                                           
         BCTR  R1,0                DON'T PUT '+' IN RECORD KEY                  
VCOD036  DS    0H                                                               
         CH    R1,=H'3'                                                         
         BH    ERREND              CODE TOO LONG                                
         USING ROWNREC,R6                                                       
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,AGENCY                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ROWNKOWN(0),12(R4)                                               
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD040  DS    0H                                                               
         CLI   SUBSCRN,X'40'       STATION                                      
         BNE   VCOD050                                                          
         CLI   0(R4),6                                                          
         BH    ERREND              CODE TOO LONG                                
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
VCOD043  DS    0H                                                               
         LA    R3,12(R1,R4)                                                     
         CLI   0(R3),C'-'                                                       
         BE    VCOD045                                                          
         BCT   R1,VCOD043                                                       
         MVC   RSTAKSTA,12(R4)                                                  
         B     VCOD100                                                          
VCOD045  DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTAKSTA(0),12(R4)                                               
         OC    RSTAKSTA,SPACES                                                  
         CLI   1(R3),C'T'                                                       
         BE    VCOD100                                                          
         MVC   RSTAKSTA+4(1),1(R3)                                              
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD050  DS    0H                                                               
         CLI   SUBSCRN,X'50'       MARKET                                       
         BNE   VCOD060                                                          
         CLI   0(R4),4                                                          
         BH    ERREND              CODE TOO LONG                                
         USING RMKTREC,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   RMKTKMKT(0),12(R4)                                               
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD060  DS    0H                                                               
         CLI   SUBSCRN,X'60'       SALESPERSON?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),3                                                          
         BH    ERREND              CODE TOO LONG                                
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,12(R4)                                                  
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD065  DS    0H                  TEAM OF S/P                                  
         CLI   0(R4),3                                                          
         BH    ERREND                                                           
         USING RTEMREC,R6          BUILD TEAM RECORD                            
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,AGENCY                                                  
         MVC   RTEMKTEM,13(R4)                                                  
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
VCOD070  DS    0H                  SETS                                         
         CLI   0(R4),5                                                          
         BH    ERREND                                                           
         LA    R3,SETTAB           FIND SET TYPE                                
         B     *+8                                                              
VCOD080  DS    0H                                                               
         LA    R3,L'SETTAB(R3)                                                  
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SUBSCREEN NOT IN TABLE                       
         CLC   SUBSCRN,0(R3)                                                    
         BNE   VCOD080                                                          
         USING RSETREC,R6          BUILD SET RECORD                             
         MVI   RSETKTYP,X'38'                                                   
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,1(R3)                                                   
         MVC   RSETKID,13(R4)                                                   
         B     VCOD100                                                          
         DROP  R6                                                               
*                                                                               
SETTAB   DS    0CL3                                                             
* XL1 SUBSCREEN/ELEMENT CODE                                                    
* CL2 CORRESPONDING SET RECORD SET TYPE,X'00' IF NO SETS                        
         DC    X'10',C'GS' GRP/SUBGRP                                           
         DC    X'20',C'OF' OFFICE                                               
         DC    X'30',2X'00'                                                     
         DC    X'40',C'ST' STATION                                              
         DC    X'50',2X'00'                                                     
         DC    X'60',C'SP' SALESPERSON                                          
         DC    X'00'                                                            
*                                                                               
VCOD100  DS    0H                  LOOKUP RECORD                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY,0                     
         CLI   8(R1),0                                                          
         BNE   ERREND                                                           
*                                                                               
         CLI   SUBSCRN,X'40'       STATION?                                     
         BNE   VCOD110                                                          
         CLI   12(R4),C'*'         STATION SET?                                 
         BE    VCOD110                                                          
         LA    R1,L'RSTAKSTA                                                    
         LA    R3,KEY+(RSTAKSTA-RSTAKEY) PLUCK CODE FROM KEY                    
         B     VCOD115                                                          
VCOD110  DS    0H                                                               
         ZIC   R1,0(R4)            L'SCANNER FIELD                              
         LA    R3,12(R4)           A(DATA)                                      
VCOD115  DS    0H                                                               
         LA    R6,ELEM                                                          
         ZIC   RF,1(R6)                                                         
         AR    RF,R6               END OF ELEM                                  
         LA    R6,3(R6)            1ST SUB ELEM                                 
VCOD120  DS    0H                                                               
         CR    R6,RF                                                            
         BNL   VCOD130                                                          
         CLM   R1,1,0(R6)                                                       
         BNE   VCOD125                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),1(R6)       ALREADY IN ELEMENT                           
         BE    ERREND              ERROR - DUPLICATE                            
         LA    R1,1(R1)                                                         
VCOD125  LA    R6,1(R1,R6)                                                      
         B     VCOD120                                                          
*                                                                               
VCOD130  DS    0H                                                               
         LA    R6,ELEM             WRITE CODE/SET TO ELEM                       
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               END OF ELEM                                  
*                                                                               
         STC   R1,0(R6)            WRITE CODE LEN                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),0(R3)                                                    
         ZIC   R6,ELEM+1           ELEM LEN                                     
         LA    R1,2(R1)                                                         
         AR    R6,R1                                                            
         STC   R6,ELEM+1                                                        
*                                                                               
         CLI   12(R4),C'*'         SET?                                         
         BNE   EXITOK                                                           
         GOTO1 (RFGETREC,REPFACS),DMCB,KEY,0,0,RFBLOCK                          
         BE    *+6                                                              
         DC    H'0'                MUST HAVE SET RECORD                         
         L     R6,DMCB+4           A(SET RECORD)                                
         USING RSETREC,R6                                                       
         CLI   RSETELEM,1                                                       
         BNE   EXITOK                                                           
         TM    RSET1FLG,X'08'      EXCLUSION SET?                               
         BO    EXITL               YES - SET CC ON EXIT                         
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
DREC     DS    0H                                                               
         BAS   RE,VSUB             CHECK SUBSCREEN SELECTORS                    
         L     R6,AIO              CONTROL FILE RECORD                          
*                                                                               
         MVC   KEYSAVE2,KEY        TO RESTORE LATER                             
*                                                                               
         XC    SC3KF1,SC3KF1       CLEAR KEY FIELDS ON SCREEN                   
         OI    SC3KF1H+6,X'80'                                                  
         XC    SC3KF2,SC3KF2                                                    
         OI    SC3KF2H+6,X'80'                                                  
*                                                                               
* PASSWORD KEY                                                                  
*                                                                               
         CLI   RECNUM,47            KEY ON PASSWORD?                            
         BNE   DREC040              NO                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVC   SC3KF1(L'SA0KCODE),SA0KCODE                                      
         DROP  R6                                                               
         MVI   ELCODE,SAPALELQ     PASSIVE POINTER ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM                          
         USING SAPALD,R6                                                        
         MVC   SC3KF2(8),SAPALPID                                               
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
         CLI   RECNUM,48            KEY ON PERSON?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SAPEREC,R6           BUILD PASSWORD KEY                          
         MVC   SC3KF1(L'SAPEPID),SAPEPID                                        
         DROP  R6                                                               
         MVI   ELCODE,SAPWDELQ     PERSON/PASSWORD ELEM                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                EXPECTING THIS ELEM TO BE THERE              
         USING SAPWDEL,R6                                                       
         MVC   SC3KF2(L'SAPWDCOD),SAPWDCOD                                      
         DROP  R6                                                               
*                                                                               
DREC050  DS    0H                                                               
         XC    SC3FNM,SC3FNM       CLEAR 1ST NAME FIELD                         
         OI    SC3FNMH+6,X'80'     XMIT                                         
         XC    SC3LNM,SC3LNM       CLEAR 1ST NAME FIELD                         
         OI    SC3LNMH+6,X'80'     XMIT                                         
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
         MVC   SC3FNM(0),1(R4)                                                  
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
         CLI   0(R4),L'SC3LNM                                                   
         BNH   *+8                                                              
         LA    R1,L'SC3LNM                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SC3LNM(0),1(R4)                                                  
         DROP  R6                                                               
*                                                                               
DREC080  DS    0H                                                               
         XC    SC3OFC,SC3OFC                                                    
         OI    SC3OFCH+6,X'80'                                                  
         XC    SC3DPT,SC3DPT                                                    
         OI    SC3DPTH+6,X'80'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,SAPERELQ     PERSONNEL DETAILS ELEM                       
         BAS   RE,GETEL                                                         
         BNE   DREC090                                                          
         USING SAPERD,R6                                                        
         MVC   SC3OFC,SAPEROFF     OFFICE                                       
         MVC   SC3DPT,SAPERDID     DEPT                                         
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
*&&DO                                                                           
         LA    R2,SC3UIDH                                                       
         XC    8(L'SC3UID,R2),8(R2)                                             
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         MVI   ELCODE,X'20'        USER ID ELEMS                                
         BAS   RE,GETEL                                                         
         BE    DREC100                                                          
         MVC   8(3,R2),=C'ALL'                                                  
         MVI   5(R2),3                                                          
         B     DREC140                                                          
DREC100  DS    0H                                                               
         LA    R4,8(R2)                                                         
         LA    R5,L'SC3UID+8(R2)   END OF FIELD                                 
         B     *+12                                                             
DREC110  DS    0H                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R1,L'SAID           FIGURE LEN OF ID TEXT                        
         LA    R3,L'SAID+2(R6)                                                  
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-10             R1=LEN OF ID                                 
         LA    RF,2(R1,R4)                                                      
         CR    RF,R5                                                            
         BNL   DREC120             NO ROOM  FOR ',+' AFTER CODE                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R6)                                                    
         CLC   =X'0000',0(R4)      LIST?                                        
         BNE   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         LA    R4,1(R1,R4)         INCREMENT POINTER                            
         BAS   RE,NEXTEL                                                        
         BE    DREC110             LOOP                                         
         B     DREC130                                                          
DREC120  DS    0H                  NO SPACE LEFT, PUT +                         
         MVI   0(R4),C'+'                                                       
         LA    R4,1(R4)                                                         
DREC130  DS    0H                                                               
         LA    RF,8(R2)                                                         
         SR    R4,RF                                                            
         STC   R4,5(R2)            TEXT LEN                                     
*&&                                                                             
*                                                                               
* READ REP SECURITY RECORD                                                      
*                                                                               
DREC140  DS    0H                                                               
         LA    R6,KEY              READ REP SEC RECORD                          
         XC    KEY,KEY                                                          
         USING RSECREC,R6                                                       
         MVC   RSECKTYP(2),=X'1501'                                             
         MVC   RSECKREP,AGENCY                                                  
         MVC   RSECKPID,PIDNUM                                                  
         L     R6,AIO2                                                          
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO2,0,RFBLOCK                       
         BE    *+6                                                              
         SR    R6,R6               NO RECORD INDICATOR                          
*                                                                               
         BAS   RE,DISCODES         DISPLAY CODES                                
*                                                                               
         MVC   KEY,KEYSAVE2        RESTORE REC WE CAME IN WITH                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    XIT                                                              
         DC    H'0'                                                             
****************************************************************                
*                        LIST ROUTINE                          *                
****************************************************************                
LIST     DS    0H                                                               
         LA    R6,KEY                                                           
         OC    KEY,KEY             INITIALIZED                                  
         BZ    LIST010             NO                                           
*                                                                               
         CLI   RECNUM,47           CHECK IF REC CHANGED & WE NEED TO            
         BNE   *+12                RE INIT LIST KEY                             
         CLI   KEY,SA0KTYPQ                                                     
         BNE   LIST010                                                          
         CLI   RECNUM,48                                                        
         BNE   LIST030                                                          
         CLI   KEY,SAPETYPQ                                                     
         BE    LIST030                                                          
*                                                                               
LIST010  DS    0H                  BUILD LIST KEY                               
         XC    KEY,KEY                                                          
*                                                                               
         CLI   RECNUM,47           KEY ON PASSWORD?                             
         BNE   LIST015                                                          
         USING SA0REC,R6            BUILD PASSWORD KEY                          
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVI   SA0KCODE,1           FOR LIST, START AT 1ST PASSWORD             
         CLI   SC2KF1H+5,0         ANY START AT?                                
         BE    LIST020             NO                                           
         MVC   SA0KCODE(L'SC2KF1),SC2KF1                                        
         B     LIST020                                                          
         DROP  R6                                                               
*                                                                               
LIST015  DS    0H                                                               
         CLI   RECNUM,48           KEY ON PERSON? ?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SAPEREC,R6                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECAGY                                                   
         CLI   SC2KF1H+5,0         ANY START AT?                                
         BE    LIST020             NO                                           
         MVC   SAPEPID,SC2KF1                                                   
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
         CLI   RECNUM,47           KEY ON PASSWORD?                             
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
         CLI   RECNUM,48           KEY ON PERSON?                               
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
         CLI   SC2OFCH+5,0         OFFICE FILTER?                               
         BE    LIST260             NO                                           
         CLC   LOFF,SC2OFC         MATCH FILTER?                                
         BNE   LIST350             NO                                           
LIST260  DS    0H                                                               
         CLI   SC2DPTH+5,0         DEPT FILTER?                                 
         BE    LIST270             NO                                           
         CLC   LDPT,SC2DPT         MATCH FILTER?                                
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
         LA    R2,LRESTR           OUTPUT AREA                                  
         BAS   RE,DISREST          DISPLAY RESTRICTIONS                         
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
* DISCODES - DISPLAY CODES SECTION OF SCREEN                                    
*            R6: A(REP SEC RECORD) OR NULLS                                     
***********************************************************************         
DISCODES NTR1                                                                   
         GOTO1 CALLOV,DMCB,(X'A5',SC3LAST),0  DATA SEC SCREEN                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETSCRN          SETUP SCREEN                                 
         BAS   RE,DISPF12          DISP PF12 LABEL (WHEN APPROPRIATE)           
*                                                                               
         MVC   ELCODE,SUBSCRN      ELEM NUM FROM SUBSCREEN CODE                 
         B     DCOD150                                                          
*                                                                               
* LOOP THRU SCREEN FIELDS, GET ELEM FOR EACH FIELD                              
*                                                                               
* SCREEN FIELDS TO DISPLAY                                                      
*   A  A(FIELD HEADER)                                                          
*   X  SUB RECORD CODE IN ELEMENT                                               
*   X  X'00' SUBSIDIARY FIELD, X'01' MASTER FIELD                               
SCRNFLDS DS    0CL6                                                             
         DC    AL4(SC3VA1H-SCRNTOP),X'01',X'00' SUBSIDIARY FIELDS               
         DC    AL4(SC3NC1H-SCRNTOP),X'02',X'00'                                 
         DC    AL4(SC3NS1H-SCRNTOP),X'03',X'00'                                 
         DC    AL4(SC3NM1H-SCRNTOP),X'04',X'00'                                 
         DC    AL4(SC3NO1H-SCRNTOP),X'05',X'00'                                 
         DC    AL4(SC3NW1H-SCRNTOP),X'06',X'00'                                 
         DC    AL4(SC3NL1H-SCRNTOP),X'07',X'00'                                 
         DC    AL4(SC3VA2H-SCRNTOP),X'01',X'01' MASTER FIELDS                   
         DC    AL4(SC3NC2H-SCRNTOP),X'02',X'01'                                 
         DC    AL4(SC3NS2H-SCRNTOP),X'03',X'01'                                 
         DC    AL4(SC3NM2H-SCRNTOP),X'04',X'01'                                 
         DC    AL4(SC3NO2H-SCRNTOP),X'05',X'01'                                 
         DC    AL4(SC3NW2H-SCRNTOP),X'06',X'01'                                 
         DC    AL4(SC3NL2H-SCRNTOP),X'07',X'01'                                 
         DC    4X'00'                                                           
*                                                                               
DCOD150  DS    0H                                                               
         LA    R3,SCRNFLDS         SCREEN FIELDS                                
DCOD200  DS    0H                                                               
         ZICM  R2,0(R3),4          SCREEN FIELD DISPLACEMENT                    
         BZ    DCOD500             DONE                                         
         AR    R2,RA               FIELD ADDRESS                                
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    DCOD290             DON'T DISPLAY ANYTHING IN IT                 
*                                                                               
         XC    8(L'SC3VA1,R2),8(R2) CLEAR FIELD                                 
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LTR   R6,R6               DID WE GET A RECORD BEFORE?                  
         BZ    DCOD220             NO - CAN'T HAVE ELEMENTS                     
*                                                                               
         OC    ELCODE,5(R3)        ADJUST FOR MASTER/SUBSIDIARY                 
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(ELCODE,AIO2),(1,4(R3)),0           
         CLI   12(R1),0            GOT IT?                                      
         BE    DCOD230             YES                                          
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DCOD220  DS    0H                                                               
         CLI   4(R3),1             VALID ACCESS (S1) FIELD?                     
         BNE   DCOD290             NO                                           
         MVC   8(3,R2),=C'ALL'     DEFAULT TO ALL                               
         MVI   5(R2),3                                                          
         B     DCOD290             NEXT FIELD                                   
*                                                                               
* LOOP THRU ELEM, PUT CODES TO SCREEN FIELD                                     
*                                                                               
DCOD230  DS    0H                                                               
         ICM   R6,15,12(R1)        A(ELEM)                                      
         ZIC   R0,1(R6)                                                         
         AR    R0,R6               A(END OF ELEM)                               
         LA    R4,8(R2)            BEGINNING OF OUTPUT FIELD                    
         LA    R6,RSECSCCL-RSECSCEL(R6)  BEGINNING OF MINI ELEM                 
         B     DCOD250                                                          
*                                                                               
DCOD240  DS    0H                                                               
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         ZIC   RF,5(R2)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
DCOD250  DS    0H                                                               
         CLI   SUBSCRN,X'40'       STATION?                                     
         BNE   DCOD255                                                          
         CLI   1(R6),C'*'          STATION SET?                                 
         BE    DCOD255             NORMAL DISPLAY                               
         GOTOX (RFSTAOUT,REPFACS),DMCB,1(R6),(1,0(R4))                          
***>     GOTOX (RFSTAOUT,REPFACS),DMCB,1(R6),0(R4)                              
         ZIC   R1,DMCB+4                                                        
         AR    R4,R1               NEXT SPOT IN SCREEN FIELD                    
         ZIC   RF,5(R2)                                                         
         AR    RF,R1               INCREMENT INPUT LEN                          
         STC   RF,5(R2)                                                         
         SR    R1,R1                                                            
         ICM   R1,1,0(R6)          CODE LEN                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         B     DCOD260                                                          
DCOD255  DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R6)          CODE LEN                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),1(R6)       CODE TO SCREEN                               
         LA    R4,1(R1,R4)         NEXT SPOT IN SCREEN FIELD                    
         ZIC   RF,5(R2)                                                         
         LA    RF,1(R1,RF)         INCREMENT INPUT LEN                          
         STC   RF,5(R2)                                                         
DCOD260  DS    0H                                                               
         CLI   5(R2),L'SC3VA1                                                   
         BNH   *+6                                                              
         DC    H'0'                BLEW PAST END OF FIELD                       
         LA    R6,2(R1,R6)         NEXT MINI ELEM                               
         CR    R6,R0               PAST END OF ELEM?                            
         BL    DCOD240             NO - LOOP                                    
*                                                                               
DCOD290  DS    0H                                                               
         LA    R3,L'SCRNFLDS(R3)    NEXT FIELD IN TABLE                         
         B     DCOD200                                                          
*                                                                               
DCOD500  DS    0H                  RESTORE RECORD WE CAME IN WITH               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   DISPREST - DISPLAY RESTRICTIONS ON LIST LINE                                
*        R2 MUST ADDRESS 'LRESTR'                                               
***********************************************************************         
DISREST  NTR1                                                                   
         LA    R6,KEY              READ REP SEC RECORD                          
         XC    KEY,KEY                                                          
         USING RSECREC,R6                                                       
         MVC   RSECKTYP(2),=X'1501'                                             
         MVC   RSECKREP,AGENCY                                                  
         MVC   RSECKPID,PIDNUM                                                  
         L     R6,AIO3                                                          
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO3,0,RFBLOCK                       
         BE    *+6                                                              
         SR    R6,R6               NO RECORD                                    
*                                                                               
         LA    R3,RESTAB                                                        
DREST020 DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         ZIC   R4,2(R3)                                                         
         AR    R4,R2                                                            
         MVI   0(R4),C'-'          DEFAULT                                      
         LTR   R6,R6                                                            
         BZ    DREST050            NO RECORD, NO RESTRICTIONS                   
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(0(R3),AIO3),0,0                    
         CLI   12(R1),0            GOT ELEM?                                    
         BNE   DREST030            NO                                           
         MVI   0(R4),C'S'          SUBSID RESTRICTED                            
*                                                                               
DREST030 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(1(R3),AIO3),0,0                    
         CLI   12(R1),0            GOT ELEM?                                    
         BNE   DREST050            NO                                           
         CLI   0(R4),C'S'                                                       
         BNE   DREST040                                                         
         MVI   0(R4),C'M'          MASTER RESTRICTED                            
         B     DREST050                                                         
DREST040 DS    0H                                                               
         MVI   0(R4),C'B'          BOTH RESTRICTED                              
*                                                                               
DREST050 DS    0H                                                               
         LA    R3,L'RESTAB(R3)                                                  
         B     DREST020                                                         
*                                                                               
* RESTRICTIONS SUMMARY DISPLAY CONTROL TABLE                                    
*   BYTE 1-2 SUBSID/MASTER ELEM CODE FOR LEVEL                                  
*   BYTE  3  DISPL INTO DISPLAY FIELD TO OUTPUT RESULT                          
*                                                                               
RESTAB   DS    0CL3                RESTRICTIONS TABLE                           
         DC    X'1011',AL1(1)      GROUP                                        
         DC    X'3031',AL1(5)      OWNER                                        
         DC    X'5051',AL1(9)      MARKET                                       
         DC    X'4041',AL1(13)     STATION                                      
         DC    X'2021',AL1(17)     OFFICE                                       
         DC    X'6061',AL1(21)     SALESPERSON                                  
         DC    X'7070',AL1(25)     PROGRAMS                                     
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*   VSUB - VALIDATE SUBSCREEN SELECTOR FIELDS                                   
***********************************************************************         
VSUB     NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         LA    R4,SELFLDS          START OF TABLE                               
         MVI   SUBSCRN,0           CLEAR SELECTOR                               
         B     *+8                                                              
VSUB0060 DS    0H                                                               
         LA    R4,L'SELFLDS(R4)    NEXT TABLE ENTRY                             
         ICM   R2,15,0(R4)         FIELD DISPLACEMENT                           
         BZ    VSUB0080            END OF TABLE                                 
         AR    R2,RA               FIELD HEADER ADDRESS                         
         TM    4(R2),X'80'         MODIFIED?                                    
         BZ    VSUB0070            NO                                           
         CLI   5(R2),0                                                          
         BE    VSUB0070                                                         
         CLI   8(R2),C'X'                                                       
         BNE   ERREND                                                           
         MVC   SUBSCRN,4(R4)                                                    
         B     VSUB0080                                                         
VSUB0070 DS    0H                                                               
         CLI   8(R2),C'X'                                                       
         BNE   VSUB0060                                                         
         MVC   SUBSCRN,4(R4)                                                    
         B     VSUB0060                                                         
*                                                                               
VSUB0080 DS    0H                                                               
         CLI   SUBSCRN,0                                                        
         BNE   *+8                                                              
         MVI   SUBSCRN,X'10'       DEFAULT TO GROUP                             
*                                                                               
         LA    R4,SELFLDS          START OF TABLE                               
         B     *+8                                                              
VSUB0090 DS    0H                                                               
         LA    R4,L'SELFLDS(R4)    NEXT TABLE ENTRY                             
         ICM   R2,15,0(R4)                                                      
         BZ    VSUB0100                                                         
         AR    R2,RA                                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         MVI   8(R2),0                                                          
         CLC   SUBSCRN,4(R4)                                                    
         BNE   VSUB0090                                                         
         MVI   5(R2),1                                                          
         MVI   8(R2),C'X'                                                       
         B     VSUB0090                                                         
VSUB0100 DS    0H                                                               
         B     XIT                                                              
*                                                                               
* SUBSCREEN SELECTOR FIELDS                                                     
*   A  DISPLACEMENT OF FIELD HEADER                                             
*   X  ELEM CODE OF SUBSIDIARY STYLE ELEM                                       
SELFLDS  DS    0CL5                                                             
         DC    AL4(SC3GOPH-SCRNTOP),X'10'   GROUP                               
         DC    AL4(SC3OOPH-SCRNTOP),X'20'   OFFICE                              
         DC    AL4(SC3WOPH-SCRNTOP),X'30'   OWNER                               
         DC    AL4(SC3SOPH-SCRNTOP),X'40'   STATION                             
         DC    AL4(SC3MOPH-SCRNTOP),X'50'   MARKET                              
         DC    AL4(SC3LOPH-SCRNTOP),X'60'   SALESPERSON                         
***>>    DC    AL4(SC3POPH-SCRNTOP),X'70'   PROGRAMS                            
         DC    4X'00'                                                           
*                                                                               
         DROP  RB                                                               
***********************************************************************         
*   DISPF12 - DISPLAY PF12 LABEL WHEN APPROPRIATE                               
***********************************************************************         
DISPF12  NTR1                                                                   
         LA    R2,SC3PFKH                                                       
         CLI   SUBSCRN,X'70'       PGM SEC DISPLAY?                             
         BNE   *+8                                                              
         LA    R2,SC6PFKH                                                       
DPF12010 DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DPF12020                                                         
         NI    1(R2),X'FF'-X'04'  HIGH INTENSITY                                
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
DPF12020 DS    0H                                                               
         OI    1(R2),X'04'     LOW INTENSITY                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
*=============================================================*                 
* SETSCRN - SETUP SCREEN                                                        
*=============================================================*                 
SETSCRN  NTR1                                                                   
         XC    KEY,KEY             READ REP RECORD                              
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVC   AIO,AIO1                                                         
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO3,0,RFBLOCK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         CLC   =X'0000',RREPMAST   MASTER/SUBSIDIARY?                           
         BE    DONS060             NO MASTER INFO ON SCREEN                     
         CLC   =X'4040',RREPMAST   MASTER/SUBSIDIARY?                           
         BNE   XIT                 NO MASTER INFO ON SCREEN                     
         DROP  R6                                                               
*                                                                               
DONS060  DS    0H                  REMOVE MASTER FIELDS                         
         LA    R3,MASTHIDE                                                      
DONS070  DS    0H                                                               
         ICM   R2,15,0(R3)                                                      
         BZ    DONS080                                                          
         AR    R2,RA                                                            
         OI    6(R2),X'80'         XMIT FIELD                                   
         TM    4(R3),X'01'         INPUT FIELD?                                 
         BZ    *+8                                                              
         OI    1(R2),X'20'         PROTECT IT                                   
         TM    4(R3),X'02'         LABEL?                                       
         BZ    *+8                                                              
         OI    1(R2),X'0C'         HIDE IT                                      
         LA    R3,L'MASTHIDE(R3)    NEXT FIELD IN TABLE                         
         B     DONS070                                                          
*                                                                               
DONS080  DS    0H                                                               
         MVC   SC3LA1,ACCMSG       CHANGE SUBSIDIARY SECTION LABEL              
         OI    SC3LA1H+6,X'80'                                                  
         B     XIT                                                              
*                                                                               
ACCMSG   DC    CL(L'SC3LA1)'ACCESS LEVEL     Codes,*Sets'                       
*                                                                               
MASTHIDE DS    0CL5                MASTER FIELDS TO HIDE                        
         DC    AL4(SC3VA2H-SCRNTOP),X'01' INPUT FIELDS                          
         DC    AL4(SC3NC2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3NS2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3NM2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3NO2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3NW2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3NL2H-SCRNTOP),X'01'                                       
         DC    AL4(SC3MLLH-SCRNTOP),X'02' LABELS                                
         DC    AL4(SC3VALH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NCLH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NSLH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NMLH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NOLH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NWLH-SCRNTOP),X'02'                                       
         DC    AL4(SC3NLLH-SCRNTOP),X'02'                                       
         DC    4X'00'                                                           
*                                                                               
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
       ++INCLUDE REPSECTAB                                                      
*                                                                               
         LTORG                                                                  
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
         DS    CL2                                                              
LRESTR   DS    CL27                                                             
         DS    CL12                                                             
*                                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMD9D                                                                      
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
       ++INCLUDE RESFMAED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMAFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMA1D                                                       
         EJECT                                                                  
         ORG   SC3LAST                                                          
       ++INCLUDE RESFMA5D                                                       
         EJECT                                                                  
         ORG   SC3LAST                                                          
       ++INCLUDE RESFMA6D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
KEYSAVE2 DS    CL27                                                             
*                                                                               
SUBSCRN  DS    X                   SUBSCREEN TYPE                               
SCLINES  DS    X                   NUMBER OF SCANNER LINES                      
*                                                                               
SECAGY   DS    CL2                 AGENCY FOR SECURITY RECORDS                  
PIDNUM   DS    CL2                                                              
*                                                                               
SECRECD  DSECT                                                                  
       ++INCLUDE REGENSEC                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
**                                                                              
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENOFF                                                       
       ++INCLUDE REGENOWN                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENTEM                                                       
**                                                                              
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202RESFM1A   04/25/01'                                      
         END                                                                    
