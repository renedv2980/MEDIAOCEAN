*          DATA SET DDGETDARE  AT LEVEL 161 AS OF 05/23/19                      
*PHASE T00A3EC                                                                  
***********************************************************************         
* P1 BYTE  0   = C'R' - EDI PARTNER CODE LOOKUP                                 
*                C'U' - EDI USERID LOOKUP                                       
*                C'L' - LOCAL STATION USERID LOOKUP                             
*                C'A' - AGENCY USERID LOOKUP                                    
* P1 BYTES 1-2 = OFFICE CODE IF APPLICABLE                                      
* P1 BYTE  3   = AFTER GETDARE SET TO ERROR CONDITION, CC=NEQ                   
*                   CODE = 1    INVALID LOOKUP CODE P1-BYTE0                    
*                   CODE = 2    MISSING 3 CHAR REP CODE                         
*                   CODE = 3    MISSING REP PREFIX                              
*                   CODE = 4    MISSING AGENCY/STATION ID                       
*                   CODE = 5    MISSING STORAGE BLOCK LENGTH                    
*                   CODE = 10   MISSING DARE VENDOR IN IDI RECORD               
*                                ONLY SET FOR STATION OR AGENCY LOOKUP          
*                                IT IS NOT REQUIRED FOR REP LOOKUP              
*                   CODE = 11   EDI PARTNER RECORD WAS NOT FOUND                
*                                                                               
* P2 BYTES 0-3 = AL4(3 CHAR CODE) IF 'C'ODE LOOKUP                              
*              = AL4(USERID PREFIX) IF 'U'SER ID LOOKUP                         
*              = AL4(STATION) IF 'S'TATION ID LOOKUP                            
*                                                                               
* P3 BQTES 0-3 = A(STORAGE BLOCK)                                               
*                                                                               
* P4 BYTES 0-3 = L'(STORAGE BLOCK)                                              
*                                                                               
* P5 BYTES 0-3 = A(DATAMGR)                                                     
*                                                                               
***********************************************************************         
         TITLE 'GET DARE ENTRY/TABLE'                                           
         PRINT NOGEN                                                            
GETDARE  CSECT                                                                  
         NMOD1 WORKX-WORKD,**GETDAR,RA,RR=R2,CLEAR=YES                          
         USING WORKD,RC            RC=A(W/S)                                    
         SAM24 ,                   SET IN 24-BIT MODE                           
*                                                                               
         ST    R2,RELO                                                          
         MVC   GDPARMS(24),0(R1)                                                
         ST    RD,SAVERD           SAVE RD FOR ERROR EXIT                       
         ST    R1,APARM                                                         
*                                                                               
         USING DAREPTD,R8          R8 = A(RETURN BLOCK)                         
         ICM   R8,15,GDASTOR                                                    
         JZ    ERRCOD5                                                          
*                                                                               
         L     R0,GDASTOR                                                       
         SR    R1,R1                                                            
         ICM   R1,15,GDLSTOR                                                    
         JZ    ERRCOD5                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               INITIALIZE OUTPUT DARE BLOCK                 
*                                                                               
         AR    R0,R1                                                            
         STCM  R0,15,AENDBLK       SAVE ADDRESS OF END OF BLOCK                 
*                                                                               
         LA    RF,IO                                                            
         ST    RF,AIO1                                                          
         LAY   RF,IO2                                                           
         ST    RF,AIO2                                                          
*                                                                               
         ICM   R3,15,GDACODE       DID YOU PASS GDACODE?                        
         JZ    *+2                  NO, YOU NEED TO PASS THIS                   
*                                                                               
         CLI   GDTYPE,GDTCODE      'R'EPCODE LOOKUP?                            
         JNE   GETD010                                                          
         MVI   DAPTTYP,CTEVKRPQ    C'R' - REP LOOKUP                            
         CLC   0(L'DAPTCODE,R3),=CL10' '   DID WE GET SOMETHING?                
         JNH   ERRCOD2                                                          
GETD005  MVC   DAPTCODE,0(R3)         SAVE 3 CHAR REP CODE                      
         J     GETD040                                                          
*                                                                               
GETD010  CLI   GDTYPE,GDTUSER      'U'SERID LOOKUP?                             
         JNE   GETD020                                                          
         MVI   DAPTTYP,CTEVKRPQ    C'R' - REP LOOKUP                            
         CLC   0(L'DAPTRPPF,R3),=CL10' '   DID WE GET SOMETHING?                
         JNH   ERRCOD3                                                          
         MVC   DAPTRPPF,0(R3)                                                   
         J     GETD040                                                          
*                                                                               
GETD020  MVI   DAPTTYP,CTEVKLSQ    C'L' - LOCAL STATION LOOKUP                  
         CLI   GDTYPE,GDTSTTN                                                   
         JE    GETD030                                                          
         MVI   DAPTTYP,CTEVKAGQ    C'A' - AGENCY LOOKUP                         
         CLI   GDTYPE,GDTAGY                                                    
         JNE   ERRCOD1                                                          
*                                                                               
GETD030  CLC   0(L'DAPTSTTN,R3),=CL10' '  DID WE GET SOMETHING?                 
         JNH   ERRCOD4                                                          
         MVC   DAPTSTTN,0(R3)      SAVE THE STATION/AGENCY                      
         BRAS  RE,STAGLKUP         STATION/AGENCY IDI LOOKUP                    
         JNE   EXITN                                                            
*** COMMENT OUT LINES BELOW WHEN WE START READING STA/AGY PARTNER RECS          
         J     GETD050                                                          
*                                                                               
GETD040  BRAS  RE,RSALKUP          REP/STA/AGY EDI LOOKUP                       
         JNE   EXITN                                                            
*                                                                               
GETD050  DS    0H                                                               
         BRAS  RE,GETVEND                                                       
*                                                                               
         L     RF,ANXTFEAT                                                      
         SR    RF,R8                                                            
         STCM  RF,3,DAPTLEN        SET LENGTH OF OUTPUT BLOCK                   
*                                                                               
         J     EXITY                                                            
*                                                                               
XMOD     XMOD1 1                                                                
*                                                                               
ERRCOD1  MVI   ERRCODE,1           INVALID LOOKUP CODE                          
         J     EXITN                                                            
ERRCOD2  MVI   ERRCODE,2           MISSING 3 CHAR REP CODE                      
         J     EXITN                                                            
ERRCOD3  MVI   ERRCODE,3           MISSING REP PREFIX                           
         J     EXITN                                                            
ERRCOD4  MVI   ERRCODE,4           MISSING AGENCY/STATION                       
         J     EXITN                                                            
ERRCOD5  MVI   ERRCODE,4           MISSING STORAGE BLOCK LENGTH                 
*                                                                               
EXITN    CLI   ERRCODE,0                                                        
         BE    EXITN1                                                           
         L     R1,APARM                                                         
         MVC   3(1,R1),ERRCODE                                                  
*                                                                               
EXITN1   LHI   RE,0                                                             
         J     *+8                                                              
EXITY    LHI   RE,1                                                             
         CHI   RE,1                                                             
EXIT     DS    0H                                                               
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* EDI REP/STATION/AGENCY DARE PARTNER LOOKUP                                    
***********************************************************************         
RSALKUP  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         CLI   GDTYPE,GDTUSER      'U'SERID PREFIX LOOKUP?                      
         JNE   RSALK020                                                         
         USING CTEQKEY,R4                                                       
         MVI   CTEQKTYP,CTEQKTYQ   X'00'                                        
         MVI   CTEQKSTY,CTEQKSTQ   X'3A'                                        
         MVC   CTEQKPAR,DAPTRPPF   USERID LOOKUP                                
*                                                                               
         LA    RF,CTEQKPAR+L'CTEQKPAR-1                                         
RSALK010 CLI   0(RF),C' '                                                       
         JNE   *+8                                                              
         JCT   RF,RSALK010                                                      
         SHI   RF,1                                                             
         MVC   SVUSROFF,0(RF)      SAVE POSSIBLE USERID OFFICE                  
         MVC   0(2,RF),=C'  '      AND NOW CLEAR THE POTENTIAL OFFICE           
         J     RSALK060                                                         
         DROP  R4                                                               
*                                                                               
         USING CTEPKEY,R4                                                       
RSALK020 MVI   CTEPKTYP,CTEPKTYQ   X'00'                                        
         MVI   CTEPKSTY,CTEPKSTQ   X'39'                                        
*                                                                               
         CLI   GDTYPE,GDTCODE      'C'ODE LOOKUP?                               
         JNE   RSALK030                                                         
         MVC   CTEPKREP,DAPTCODE   USERID LOOKUP                                
         J     RSALK060                                                         
*                                                                               
RSALK030 DS    0H                                                               
         DC    H'0'                                                             
*&&DO                                                                           
*** REMOVE LINE ABOVE WHEN WE START READING STA/AGY PARTNER RECORD              
*** AND PUT COMMENTED OUT SECTION OF BELOW CODE IN                              
         CLI   GDTYPE,GDTAGY                                                    
         JNE   RSALK040                                                         
         MVI   CTEPKRCT,CTEPKAGQ   C'A'GENCY LOOKUP                             
         MVC   CTEPKAGP,DAPTCODE   AGENCY 1ST 3 CHAR ROUTING CODE               
         J     RSALK060                                                         
*                                                                               
RSALK040 DS    0H                                                               
         CLI   GDTYPE,GDTSTTN      'S'TATION LOOKUP?                            
         JNE   *+2                                                              
         MVI   CTEPKRCT,CTEPKLSQ   C'L'OCAL STATION LOOKUP                      
         MVC   CTEPKSTA,DAPTSTTN                                                
*&&                                                                             
         DROP  R4                                                               
*                                                                               
RSALK060 MVC   KEYSAVE,KEY                                                      
         GOTOR ADATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                  
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         CLI   GDTYPE,GDTUSER      'U'SERID PREFIX LOOKUP?                      
         JNE   RSALK070                                                         
         CLC   KEY(CTEQKPAR-CTEQKEY+L'CTEQKPAR),KEYSAVE                         
         JE    RSALK090                                                         
         CLC   DAPTRPPF,KEYSAVE+CTEQKPAR-CTEQKEY                                
         JE    RSALKNO                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+CTEQKPAR-CTEQKEY(L'CTEQKEY),DAPTRPPF                         
         XC    SVUSROFF,SVUSROFF                                                
         J     RSALK060                                                         
*                                                                               
RSALK070 CLC   KEY(L'CTEPKEY),KEYSAVE                                           
         JNE   RSALKNO             RECORD NOT FOUND                             
*                                                                               
RSALK090 MVC   AIO,AIO1                                                         
         GOTOR ADATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,AIO,DMWORK            
         CLI   8(R1),0                                                          
         JNE   *+2                 NOT GOOD, DIE                                
*                                                                               
         CLI   GDTYPE,GDTUSER      'U'SERID PREFIX LOOKUP?                      
         JNE   *+10                                                             
         MVC   GDOFF,SVUSROFF      MOVE TEMP OFFICE TO PERMANENT OFF            
*                                                                               
         L     R4,AIO                                                           
         USING CTEPRECD,R4                                                      
         MVC   DAPTCODE,CTEPKREP                                                
         LA    R6,CTEPDAT                                                       
         CLI   0(R6),CTEPRELQ                                                   
         JNE   *+2                 THATS BAD, IT SHOULD BE THERE                
         USING CTEPRD,R6                                                        
         MVC   DAPTVEND,CTEPVNAM                                                
         MVC   DAPTVERS,CTEPVERS                                                
         MVC   DAPTNAME,CTEPPNAM                                                
         MVC   DAPTRPPF,CTEPPREF                                                
         MVC   DAPTLPFX,CTEPLPRF                                                
         MVC   DAPTFLG1,CTEPFLG1                                                
*                                                                               
         MVC   DAPTFLG2,CTEPFLG2                                                
         CLC   DAPTCODE,=C'EOC'    END OF CONTRACT (TV)?                        
         BE    *+10                                                             
         CLC   DAPTCODE,=C'ENC'    END OF CONTRACT (RADIO)?                     
         BNE   *+8                                                              
         OI    DAPTFLG2,DPF2EOC    TURN ON BIT                                  
*                                                                               
         MVC   DAPTMED,CTEPRMED                                                 
         DROP  R4,R6                                                            
RSALKYES J     EXITY                                                            
*                                                                               
RSALKNO  MVI   ERRCODE,11          EDI PARTNER RECORD WAS NOT FOUND             
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* STATION/AGENCY ID RECORD LOOKUP                                               
*                                                                               
* WHEN WE DECIDE TO USE EDI STA/AGY RECORD, THIS ROUTINE WILL NOT BE            
* NECESSARY FOR STATION LOOKUP                                                  
***********************************************************************         
STAGLKUP NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,DAPTSTTN                                                  
         MVC   AIO,AIO1                                                         
         GOTOR ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO                      
         CLI   8(R1),0                                                          
         JNE   *+2                 DIE, SHOULD ALWAYS BE PRESENT                
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         DROP  R4                                                               
SALK010  CLI   0(R6),0             EOR?                                         
         JE    STLKNO                                                           
         CLI   0(R6),CTUSAELQ      X'33'-US AGENCY EXTRA INFO                   
         JE    SALK020                                                          
         LLC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         J     SALK010                                                          
*                                                                               
         USING CTUSAD,R6                                                        
SALK020  DS    0H                                                               
         MVC   DAPTCODE,CTUSADRC                                                
         MVC   GDOFF,CTUSADRC+3                                                 
*                                                                               
SALK030  CLI   CTUSALEN,CTUSALN2                                                
         JNE   STLKNO                                                           
         CLC   CTUSAVEN,=XL10'0'                                                
         JE    STLKNO                                                           
         MVC   DAPTVEND,CTUSAVEN                                                
         J     EXITY                                                            
         DROP  R6                                                               
*                                                                               
STLKNO   MVI   ERRCODE,10          MISSING DARE VENDOR IN IDI RECORD            
*                                                                               
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* GET VENDOR                                                                    
***********************************************************************         
GETVEND  NTR1                                                                   
         LA    R6,DAPTFEAT                                                      
         ST    R6,ANXTFEAT         SENT START OF FEATURE LIST                   
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CTEVKEY,R4                                                       
         MVI   CTEVKTYP,CTEVKTYQ   X'00'                                        
         MVI   CTEVKSTY,CTEVKSTQ   X'38'                                        
         MVC   CTEVKVND,DAPTVEND                                                
*                                                                               
         CLI   DAPTTYP,CTEVKRPQ    C'R' - REP LOOKUP?                           
         JNE   GTVE010                                                          
         MVI   CTEVKARS,CTEVKRPQ                                                
         MVC   CTEVKUSC,DAPTCODE   MOVE REP CODE                                
         CLC   GDOFF,=X'0000'      HAVE OFFICE?                                 
         JE    GTVEHI                                                           
         MVC   CTEVKOFF,GDOFF      SET OFFICE                                   
         J     GTVEHI                                                           
*                                                                               
GTVE010  CLI   DAPTTYP,CTEVKLSQ    C'L' - LOCAL STATION LOOKUP?                 
         JNE   GTVE020                                                          
         MVI   CTEVKARS,CTEVKLSQ   LOCAL STATION                                
         MVC   CTEVKUID,DAPTSTTN   MOVE AGENCY/STATION CODE                     
         J     GTVEHI                                                           
*                                                                               
GTVE020  CLI   DAPTTYP,CTEVKAGQ    C'A' - AGENCY LOOKUP                         
         JNE   *+2                  -NOT R/L/A??? DIE                           
         MVI   CTEVKARS,CTEVKAGQ   AGENCY                                       
         MVC   CTEVKRAG,DAPTCODE   MOVE AGENCY ROUTING CODE                     
         CLC   GDOFF,=X'0000'      HAVE OFFICE?                                 
         JE    GTVEHI                                                           
         MVC   CTEVKROF,GDOFF      SET OFFICE                                   
*                                                                               
GTVEHI   MVC   KEYSAVE,KEY                                                      
         GOTOR ADATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                  
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         CLC   KEY(L'CTEVKEY-1),KEYSAVE    MATCH ON KEY MINUS SEQ#              
         JE    GTVEGET                     YES                                  
         CLI   KEYSAVE+CTEVKARS-CTEVKEY,0  LOOKING FOR VENDOR LEVEL?            
         JNE   GTVENXT                      NO, WE'RE OK                        
         DC    H'0'                        NO VENDOR LVL RECORD?? DIE!!         
*                                                                               
GTVESEQ  GOTOR ADATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYSAVE,KEY                  
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         CLC   KEY(L'CTEVKEY-1),KEYSAVE    HAVE SEQUENCE?                       
         JE    GTVECOP                      YES, GET THE FEATURES               
         CLI   KEYSAVE+CTEVKARS-CTEVKEY,0  LOOKING FOR VENDOR LEVEL?            
         JE    GTVEXY                                                           
*                                                                               
GTVENXT  MVC   KEY,KEYSAVE         RESTORE TO ORIGINAL KEY                      
*                                                                               
         CLI   CTEVKARS,GDTAGY     AGY LOOKUP?                                  
         JNE   GTVENXT5                                                         
         CLC   CTEVKROF,=X'0000'   DID WE JUST READ AGY OFFICE?                 
         JE    GTVEVEND                                                         
         XC    CTEVKROF,CTEVKROF   YES, LETS READ AGENCY LEVEL NOW              
         MVI   CTEVKSQ1,0                                                       
         J     GTVEHI                                                           
*                                                                               
GTVENXT5 CLI   CTEVKARS,GDTCODE    REP LOOKUP?                                  
         JNE   GTVEVEND                                                         
         CLC   CTEVKOFF,=X'0000'   DID WE JUST READ REP OFFICE?                 
         JE    GTVEVEND                                                         
         XC    CTEVKOFF,CTEVKOFF   YES, LETS READ REP LEVEL NOW                 
         MVI   CTEVKSQ1,0                                                       
         J     GTVEHI                                                           
*                                                                               
*    IF WE GET HERE, IT MEANS WE WANT TO READ VENDOR LEVEL NOW                  
*                                                                               
GTVEVEND XC    KEY+CTEVKARS-CTEVKEY(L'CTEVKEY-CTEVKARS+CTEVKEY),KEY+CTE+        
               VKARS-CTEVKEY       CLEAR ALL EXCEPT VENDOR                      
         J     GTVEHI                                                           
*                                                                               
GTVEGET  MVC   AIO,AIO1                                                         
         GOTOR ADATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,AIO,DMWORK            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
GTVECOP  BRAS  RE,COPYFEAT                                                      
         CLI   KEYSAVE+CTEVKARS-CTEVKEY,0   JUST READ VENDOR LEVEL?             
         JNE   GTVENXT                       NO, GO READ NEXT                   
*                                                                               
GTVEXY   J     EXITY                                                            
***********************************************************************         
* COPY FEATURE AND MESSAGES TO OUTPUT BLOCK                                     
***********************************************************************         
COPYFEAT NTR1                                                                   
         L     R4,AIO                                                           
         LA    R6,CTEVDAT-CTEVRECD(R4)                                          
COFE010  CLI   0(R6),0             END-OF-REC?                                  
         JE    COFEXY                                                           
         CLI   0(R6),X'02'         FEATURE ELEMENT?                             
         JE    COFE030                                                          
COFE020  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     COFE010                                                          
*                                                                               
         USING CTEVRD,R6                                                        
COFE030  TM    CTEVFLAG,CTEVFEXC   EXCLUDE FEATURE?                             
         JNZ   COFEDEL              GO BUILD EXCLUDE LIST                       
FEAT     USING DAPTFEAT,R7                                                      
         L     R7,ANXTFEAT         A(NXT FEATURE IN BLOCK)                      
*                                                                               
*********                                                                       
* ADD FEATURE AND MESSAGE TO OUTPUT BLOCK                                       
*********                                                                       
COFEADD  BRAS  RE,CHKEXCL          CHECK MESSAGE IN EXCLUDE LIST                
         JE    COFE020              SKIP IF IT IS                               
*                                                                               
         MVC   FEAT.DAPTFEAT,CTEVFEAT     (OFF R7)                              
*                                                                               
         BRAS  RE,GETFMSG          READ FEATURE REC AND GET MESSAGES            
         JNE   COFEA010              FEATURE HAS NO MESSAGES, SKIP BUMP         
*                                                                               
         LLH   RE,DAPTNUMF         BUMP # OF FEATURES (OFF R8)                  
         AHI   RE,1                                                             
         STCM  RE,3,DAPTNUMF                                                    
*                                                                               
COFEA010 CLC   ANXTFEAT,AENDBLK    DO WE NEED MORE BLOCK STORAGE?               
         JNH   COFE020                                                          
         DC    H'0'                RAN OUT OF SPACE IN STORAGE BLOCK            
         DROP  FEAT                                                             
*                                                                               
*********                                                                       
* ADD FEATURE TO EXCLUDE LIST                                                   
*********                                                                       
COFEDEL  BRAS  RE,CHKEXCL          CHECK MESSAGE IN EXCLUDE LIST                
         JE    COFE020                                                          
         MVC   0(2,R1),CTEVFEAT                                                 
         J     COFE020                                                          
*                                                                               
COFEXY   J     EXITY                                                            
*                                                                               
***********************************************************************         
* CHECK EXCLUDE LIST                                                            
* ON EXIT:                                                                      
* - CC EQ - MESSAGE ALREADY IN LIST                                             
* - CC NEQ - MESSAGE NOT FOUND                                                  
*    R1 = A(INSERTION LOCATION)                                                 
***********************************************************************         
CHKEXCL  DS    0H                                                               
         LA    R1,EXCLIST                                                       
         LA    RF,EXCLISTX                                                      
CKEX010  CR    R1,RF                                                            
         JNH   *+6                                                              
         DC    H'0'                NEED TO MAKE EXCLIST BIGGER                  
*                                                                               
         CLC   0(2,R1),=X'0000'                                                 
         JNE   CKEX020                                                          
         CHI   R1,0                SET NEQ CC                                   
         BR    RE                                                               
*                                                                               
CKEX020  CLC   CTEVFEAT,0(R1)      ALREADY IN LIST?                             
         BER   RE                  YUP, SKIP IT                                 
         AHI   R1,L'EXCLIST        BUMP IT                                      
         J     CKEX010                                                          
*                                                                               
***********************************************************************         
* GET FEATURE MESSAGES                                                          
*                                                                               
*  ON ENTRY : ANXTFEAT  A(NEXT FEATURE IN RETURN BLOCK)                         
*                                                                               
*  ON EXIT  : ANXTFEAT  NEW A(NEXT FEATURE IN RETURN BLOCK)                     
*             CC = NEQ  NO MESSAGES, ANXTFEAT NOT TOUCHED                       
***********************************************************************         
GETFMSG  NTR1                                                                   
         L     R7,ANXTFEAT         A(NXT FEATURE IN BLOCK)                      
FEAT     USING DAPTFEAT,R7                                                      
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CTEFKEY,R4                                                       
         MVI   CTEFKTYP,CTEFKTYQ   X'00'                                        
         MVI   CTEFKSTY,CTEFKSTQ   X'3B'                                        
         MVC   CTEFKNUM,FEAT.DAPTFEAT                                           
*                                                                               
         GOTOR ADATAMGR,DMCB,=C'DMREAD',=C'GENDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         GOTOR ADATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,AIO2,DMWORK           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         LA    RF,FEAT.DAPTTEXT   RF = A(NXT MESSAGE IN BLOCK)                  
MSSG     USING DAPTTEXT,RF          BE CAREFUL, DON'T CLOBBER RF                
*                                                                               
         L     R4,AIO2                                                          
         LA    R6,CTEFDAT-CTEFRECD(R4)                                          
GEFM010  CLI   0(R6),0             END-OF-REC?                                  
         JE    GEFMX                                                            
         CLI   0(R6),X'04'         MESSAGE ELEM?                                
         JE    GEFM030                                                          
GEFM020  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     GEFM010                                                          
*                                                                               
         USING CTMSGD,R6                                                        
GEFM030  MVC   MSSG.DAPTTEXT,CTMSGTXT                                           
         MVC   MSSG.DAPTCTXT,CTMSGCTX                                           
***NOP   MVC   MSSG.DAPTMXL,????      *** WE DON'T HAVE THIS FIELD***           
*                                                                               
         LLH   RE,FEAT.DAPTNUMM    INC NUMBER OF MESSAGES (OFF R7)              
         AHI   RE,1                                                             
         STCM  RE,3,FEAT.DAPTNUMM                                               
*                                                                               
         AHI   RF,DAPTMLNQ                                                      
         J     GEFM020                                                          
*                                                                               
GEFMX    SR    RE,RE                                                            
         ICM   RE,3,FEAT.DAPTNUMM  WAS THERE ANY MESSAGES?                      
         JZ    EXITN                NO, EXIT WITH CC == NE                      
         ST    RF,ANXTFEAT         A(NXT FEATURE IN BLOCK)                      
         J     EXITY                                                            
         DROP  FEAT,MSSG                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TEST END OF BUFFER                                                            
***********************************************************************         
TENDBUFF NTR1                                                                   
         J     EXITY                                                            
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
GDPARMS  DS    5F                                                               
         ORG   GDPARMS                                                          
GDTYPE   DS    C                                                                
GDTCODE  EQU   C'R'                REP CODE                                     
GDTUSER  EQU   C'U'                REP USERID                                   
GDTSTTN  EQU   C'L'                STATION                                      
GDTAGY   EQU   C'A'                AGENCY                                       
*                                                                               
GDOFF    DS    CL2                                                              
         DS    X                                                                
GDACODE  DS    A                   A(INPUT PARAMETER)                           
GDASTOR  DS    A                   A(STORAGE BLOCK)                             
GDLSTOR  DS    F                   L'STORAGE BLOCK                              
ADATAMGR DS    A                   A(COMFACS)                                   
GDPARMX  DS    0X                  END OF PARAMETER BLOCK                       
*                                                                               
SAVERD   DS    A                   SAVE RD                                      
APARM    DS    A                   SAVE ADDRESS OF PARAMETER BLOCK              
ANXTFEAT DS    A                   A(NEXT FEATURE IN BLOCK)                     
AENDBLK  DS    A                   A(END OF BLOCK)                              
*                                                                               
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DMWORK   DS    12D                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
ERRCODE  DS    X                                                                
SVUSROFF DS    CL2                 TEMPORARY OFFICE CODE                        
*                                                                               
*                                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    X                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
WORK     DS    CL64                                                             
*                                                                               
EXCLIST  DS    50CL2               50 ENTRIES                                   
EXCLISTX EQU   *                                                                
*                                                                               
DARBLOCK DS   XL1024                                                            
DARBLOKX EQU   *                                                                
*                                                                               
AIO      DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
IO       DS    XL(4*1024)                                                       
IO2      DS    XL(4*1024)                                                       
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
       ++INCLUDE DDGETDARED                                                     
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161DDGETDARE 05/23/19'                                      
         END                                                                    
