*          DATA SET PPLFM02    AT LEVEL 004 AS OF 09/06/12                      
*PHASE T40402A                                                                  
*INCLUDE HEXOUT                                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40402 - PRINT LOGICAL FILE MAINT. PRODUCT SCREEN'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/06/12 GETFACT - USE ADDRESS PASSED BACK BY GETFACT CALL               
*                                                                               
* KWAN 10/05/01 NO DIV & INTERFACE CODE CHECKINGS FOR TRAFFIC USERS             
*                                                                               
* KWAN 08/14/01 "LW ROTATION" FIELD IS LIMITED TO H9 AND SJ ONLY                
*                                                                               
* KWAN 06/15/01 NEW SCREEN FIELD "LW ROTATION" (X'40' ELEM IN PPRDREC)          
*                                                                               
* KWAN 06/04/01 NEW SCREEN FIELD "TRAFFIC?" (X'20' BIT IN PPRDSTAT)             
*                                                                               
* KWAN 03/22/01 NO-OP PRD SECURITY CODES, ADD INTERFACE CODE LOGIC              
*                                                                               
* KWAN 02/12/01 ACCOUNT OFFICE CODE                                             
*                                                                               
* KWAN 12/00    ADD NEW FIELDS, OFFICE AND TRAFFIC (PASSIVE POINTERS)           
*                                                                               
* KWAN 11/16/99 ADD NEW FIELD, EXCLUSION CLASS                                  
*                                                                               
* SMYE 11/6/95  MODIFICATION TO DELETE PRODUCT GROUP DIRECTORY ONLY             
*               RECORDS ('3B' & '3E') WHEN PRODUCT IS DELETED.                  
*                                                                               
* BPLA 12/13/94 ADJACENCY CODES (IN PPRDEXCL)                                   
*                                                                               
* LWEI 06/01/93 PST                                                             
*                                                                               
* BPLA 03/23/93 ADD CODE TO DELETE BILLS WITH EST=0                             
*               WHEN DELETING PRODUCTS                                          
*                                                                               
* LWEI 01/14/93 UDEF FIELDS                                                     
*                                                                               
* ROSA 11/16/90 ADD GST CODE FOR CANADA                                         
*                                                                               
* BPLA 03/07/89 ADD CODE FOR SECOND BILL RECEIPT NAME LINE                      
*                                                                               
* BPLA 02/17/89 ADD AOR (X'14') TO PRDTAB1 - DELETE TABLE                       
*                                                                               
*                                                                               
* ROSA 12/16/88 READ FOR DIVISION WAS INCORRECT WHEN THE BASE PGM               
*               READ HIGH FOR A CLI/PRD COMBO WHEN FUNCTION WAS AN              
*               ADD AND THE NEW PRD WAS HIGHER THAN ANY PRODUCT                 
*               ON FILE FOR THAT CLIENT.. KEY PASSED TO THIS PGM                
*               CONTAINED INVALID IFO. HAD TO BUILD NEW KEY WHEN                
*               READING FOR DIVISION..                                          
*                                                                               
*                                                                               
* ROSA 10/10/88 ONLY ALLOW JWT (JW) AND SJR (SJ) TO SEE AND ENTER               
*               OAN CODES...     ***THIS CODE REMOVED 10/26/88 ***              
*                                                                               
* ROSA 09/22/88 SAVE PRODUCT ADDRESS WHEN ADDING NEW PRODUCT--                  
*               IF NEW PRODUCT WAS ADDED AND THEN NEW EST FOR THAT              
*               PRODUCT WAS ADDED/ THE PROD ADDRESS WAS NOT BEING               
*               SAVED CAUSING DUMP IN OAN CODEIN EST PGM.                       
*                                                                               
* ROSA 09/01/88 DO NOT PRINT DESCRIPTION 'OTHER AGENCY NAME' ANNOTATIN          
*               UNLESS AGENCY IS SJ..                                           
*                                                                               
* ROSA 08/02/88 1- CHECK TO SEE IF AGENCY CODE EXISTS.  NEW RECORD              
*               CREATED (POTHAGY)                                               
*               2- PREVENT CHANGES TO AGENCY CODE IF ESTABLISHED.               
*                                                                               
* ROSA 06/01/88 ADD AGENCY OAN CODE -- AT SOME POINT IN THE FUTURE              
*               THE OAN  CODE WILL BE VERIFIED AGAINST A CONTROL FILE.          
*               CANNOT CHANGE AN OAN PRODUCT TO AN NONOAN PRODUCT               
*               **ALSO BYPASS CODE FOR 'EXCLUSION CODE'.                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
T40402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40402,R9                                                      
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING T404FFD,RA                                                       
*                                                                               
         RELOC RELO                                                             
*                                                                               
         LA    RE,IOAREA                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         MVC   PPRDKEY,KEY                                                      
         MVI   PPRDELEM+0,X'06'    WAS X'06AA' NEW ELEM LENGTH 200              
         MVI   PPRDELEM+1,X'C8'                                                 
         MVC   PPRDDIV,=4C'0'                                                   
         MVI   PPRDLEN+0,X'00'     WAS X'CB' / NEW RECORD LENGTH 233            
         MVI   PPRDLEN+1,X'E9'                                                  
         CLI   SCRNUM,X'F2'                                                     
         BE    PRD2                                                             
         MVI   DSPSW,1                                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F2'                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETUPSCR         CERTAIN FIELDS NEED TO BE PROTECTED          
*                                                                               
         CLI   WNATION,C'C'        CANADIAN                                     
         BE    OHCANADA                                                         
         XC    PRDGSTA,PRDGSTA                                                  
         XC    PRDGST,PRDGST                                                    
         FOUT  PRDGSTAH                                                         
         FOUT  PRDGSTH                                                          
         OI    PRDGSTH+1,X'20'                                                  
         XC    PRDPSTA,PRDPSTA                                                  
         XC    PRDPST,PRDPST                                                    
         FOUT  PRDPSTAH                                                         
         FOUT  PRDPSTH                                                          
         OI    PRDPSTH+1,X'20'                                                  
*                                                                               
OHCANADA DS    0H                                                               
         MVI   SCRNUM,X'F2'                                                     
         CLI   CPROFLE,C'0'                                                     
         BNE   PRD1                                                             
         XC    PRDDIVA,PRDDIVA     CLEAR DIV ANNO                               
         XC    PRDDIV,PRDDIV                                                    
         OI    PRDDIVH+1,X'20'     PROTECTION FIELD                             
         MVI   PRDDIVH+5,0                                                      
PRD1     DS    0H                                                               
         BAS   RE,SETUSER                                                       
         CLI   BACT,1                                                           
         BE    NOTDONE                                                          
         B     *+8                                                              
PRD2     BAS   RE,SETUSER                                                       
         CLI   BACT,1                                                           
         BE    PRDSCRN                                                          
         MVC   KEY+27(4),PRDADDR                                                
         BAS   RE,GETREC                                                        
PRDSCRN  XC    PRDOANN,PRDOANN                                                  
         FOUT  PRDOANNH                                                         
         FOUT  HDRPRDNH                                                         
         CLC   HDRPRDN+20(3),OANMSG                                             
         BNE   *+10                                                             
         XC    HDRPRDN+20(L'OANMSG),HDRPRDN+20                                  
         CLI   DSPSW,0                                                          
         BNE   FORMATP                                                          
*                                                                               
* PRODUCT SCREEN IN TWA SO EDIT IT UNLESS ACTION=DISPLAY                        
*                                                                               
         CLI   BACT,X'03'          DISPLAY?                                     
         BE    FORMATP                                                          
         LA    R2,PRDPRDNH                                                      
         BAS   RE,ANY                                                           
         CLC   8(6,R2),=C'DELETE'                                               
         BE    DELETE                                                           
         XC    PPRDNAME,PPRDNAME                                                
         MVC   PPRDNAME,PRDPRDN                                                 
*                                                                               
JWTTLA   LA    R2,PRDOANH                                                       
*                                                                               
*  CANNOT CHANGE OAN PRODUCT TO A NON-OAN PRODUCT                               
*                                                                               
         CLI   BACT,1              RECORD ACTION ADD?                           
         BNE   ISACHANG                                                         
         CLI   5(R2),0             ANY INPUT?                                   
         BE    MOVEAORN                                                         
         B     YMOVEAOR            ADD OAN IF VALID // SEE IF CODE OK           
ISACHANG DS    0H                                                               
         CLI   5(R2),0             ANY INPUT? IF NONE CHECK TO SEE IF           
         BE    OANINPRD            OTHER AGENCY NAME IN PRODUCT                 
         CLC   PPRDOAN,=X'0000'    IF NO OAN CODE PRESENT THEN CANNOT           
         BE    NOCANDO             CHANGE TO AN EXISTING OAN CODE               
         B     YMOVEAOR            VERIFY EXISTANCE                             
*                                                                               
OANINPRD CLC   PPRDOAN,=X'0000'    IF BIN ZERO, NO OAN CODE                     
         BE    MOVEAORN            NOTHING ENTERED                              
         FOUT  HDRMSGH,=C'*** CANNOT REMOVE OAN CODE ***',30                    
         B     ZIPOUT                                                           
NOCANDO  FOUT  HDRMSGH,=C'*** CANNOT CHANGE OAN CODE ***',30                    
         B     ZIPOUT                                                           
*                                                                               
YMOVEAOR DS    0H                  SEE IF OTHER AGY NAME REC EXISTS             
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY      AGENCY + MEDIA                               
         MVI   KEY+3,X'16'         ID                                           
         MVC   KEY+4(2),PRDOAN                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BE    YMOVEOK                                                          
         XC    PRDOANN,PRDOANN                                                  
         FOUT  PRDOANNH                                                         
         FOUT  HDRPRDNH                                                         
         CLC   HDRPRDN+20(3),OANMSG                                             
         BNE   *+10                                                             
         XC    HDRPRDN+20(L'OANMSG),HDRPRDN+20                                  
         FOUT  HDRMSGH,=C'*** OAN CODE NONEXISTANT *****',30                    
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     ZIPOUT                                                           
*                                                                               
YMOVEOK  DS    0H                                                               
         GOTO1 GET2ZIO                                                          
         LA    R2,PRDOANN                                                       
         LA    R3,ZZZIO                                                         
         USING POTHRECD,R3                                                      
         FOUT  PRDOANNH,POTHNAME,33                                             
*                                                                               
         MVC   PPRDOAN,PRDOAN                                                   
         MVC   OANMSG+21(2),PPRDOAN   MOVE                                      
         MVC   HDRPRDN+20(L'OANMSG),OANMSG                                      
         NI    HDRPRDNH+7,X'80'                                                 
         OI    HDRPRDNH+7,X'50'    NEW LENGTH OF OUTPUT                         
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GET2ZIO             READ PRD HDR INTO ZZZIO TO PRIME D/M         
*                                                                               
         B     MOVEAORN                                                         
*                                                                               
OANMSG   DC    CL30'*OTHER AGY NAME CODE=   ***'                                
*                                                                               
MOVEAORN LA    R2,PRDBRNH                                                       
         BAS   RE,ANY                                                           
         XC    PPRDBILL,PPRDBILL                                                
         MVC   PPRDBILL,PRDBRN                                                  
*                                                                               
         XC    PPRDBIL2,PPRDBIL2                                                
         LA    R2,PRDBRN2H                                                      
         CLI   5(R2),0                                                          
         BE    EDTADDR                                                          
         MVC   PPRDBIL2,PRDBRN2                                                 
*                                                                               
EDTADDR  LA    R2,PRDAL1H                                                       
         BAS   RE,ANY                                                           
         XC    PPRDLIN1,PPRDLIN1                                                
         MVC   PPRDLIN1,PRDAL1                                                  
         LA    R2,PRDAL2H                                                       
         BAS   RE,ANY                                                           
         XC    PPRDLIN2,PPRDLIN2                                                
         MVC   PPRDLIN2,PRDAL2                                                  
         LA    R2,PRDATTNH                                                      
         XC    PPRDATTN,PPRDATTN                                                
         CLI   5(R2),0                                                          
         BE    EDTADDRX                                                         
         MVC   PPRDATTN,8(R2)                                                   
EDTADDRX DS    0H                                                               
*                                                                               
         BRAS  RE,CKLWRO           CHECK FOR LEGAL WARN ROTATION ORDERS         
         BNE   INVERR                                                           
*                                                                               
******** BRAS  RE,CHKAOFF          CHECK ACCOUNT OFFICE CODE                    
*                                                                               
CKACCNT  LA    R2,PRDACCTH                                                      
         LA    R3,FLDINV                                                        
         XC    PPRDACCT,PPRDACCT                                                
         CLI   5(R2),0                                                          
         BE    CKACCNTX                                                         
         TM    4(R2),X'08'         TEST FOR NUMERICS                            
         BNO   CKPACCT             NO                                           
         BAS   RE,PACK                                                          
         ST    R0,FULL                                                          
         MVC   PPRDACCT+1(3),FULL+1                                             
         MVI   PPRDACCT,X'FF'                                                   
         B     CKACCNTX                                                         
CKPACCT  CLI   5(R2),4             IF NOT NUMERIC THEN ONLY 4 CHARS             
         BH    ERROR                                                            
         MVC   PPRDACCT,PRDACCT                                                 
         OC    PPRDACCT,=4C' '                                                  
*                                                                               
CKACCNTX DS    0H                  END OF ACCOUNT NUMBER CHECKING               
*                                                                               
******** BRAS  RE,CKOFFTRA         CHECKING PRD OFFICE AND TRAFFIC              
******** BNE   INVERR                                                           
*                                                                               
CKDIV    DS    0H                                                               
         MVI   PPRDGST,0                                                        
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   CKDIVAA                                                          
         LA    R2,PRDGSTH                                                       
         LA    R3,2                                                             
*                                                                               
* SOME SORT OF CUT OFF DATE IS NEEDED TO PREVENT CHANGING                       
* AFTER A CERTAIN DATE                                                          
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    CKDIVAA             NO- O.K                                      
         LA    RF,VALGSTC                                                       
CLI255   CLI   0(RF),255                                                        
         BE    ERROR                                                            
         CLC   0(1,RF),PRDGST                                                   
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     CLI255                                                           
         MVC   PPRDGST,PRDGST      MOVE GST TAX CODE                            
         B     CKDIVAA                                                          
*                                                                               
VALGSTC  DC    C'SXZ',X'FF'                                                     
*                                                                               
CKDIVAA  LA    R2,PRDDIVH                                                       
         TM    12(RA),X'08'        TRAFFIC USER AUTHORIZED?                     
         BO    CKDIVX              YES, NO NEED TO CHECK DIV                    
*                                                                               
         CLI   CPROFLE,C'1'        SEE IF DIVISIONS REQUIRED                    
         BE    CKDIV1                                                           
         CLI   CPROFLE,C'2'                                                     
         BNE   CKDIV9                                                           
CKDIV1   BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         LA    R3,DIVERR1                                                       
         XC    PPRDDIV,PPRDDIV                                                  
         OI    DUB+7,X'0F'                                                      
         UNPK  PPRDDIV(3),DUB+6(2)                                              
         BAS   R8,DIVREAD                                                       
         B     CKDIVX                                                           
*                                                                               
CKDIV9   XC    PRDDIVN,PRDDIVN                                                  
         FOUT  PRDDIVNH                                                         
         LA    R3,DIVERR2                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         XC    PPRDDIV,PPRDDIV                                                  
*                                                                               
CKDIVX   DS    0H                  END OF DIVISION VALIDATION                   
*                                                                               
CKUSRF   DS    0H                  CHECKING FOR USER FIELDS                     
         TM    12(RA),X'08'        AUTHORIZED?                                  
         BO    CKUSRFX             NO CHANGES MADE (FLD IS PROTECTEDD)          
*                                                                               
         LA    R6,PPRDELEM                                                      
PRDSCRN3 CLI   0(R6),0                                                          
         BE    PRDSCRN5                                                         
         CLI   0(R6),X'08'         USER FIELDS ELEM?                            
         BE    PRDSCRN4                                                         
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PRDSCRN3                                                         
*                                                                               
PRDSCRN4 GOTO1 VRECUP,DMCB,(1,PPRDREC),0(R6)                                    
*                                                                               
PRDSCRN5 LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PPRDUDEF,R4                                                      
         LA    R2,PRDDSC1H                                                      
         LA    R3,SVP1USER                                                      
         BAS   RE,EDTUSR                                                        
*                                                                               
EDT12A   MVC   PUSER1,WORK                                                      
         MVC   PRDDSC1,WORK        CLEAR OR RE-TRANSMIT FIELD                   
         OI    PRDDSC1H+6,X'80'                                                 
*                                                                               
         LA    R2,PRDDSC2H                                                      
         LA    R3,SVP2USER                                                      
         BAS   RE,EDTUSR                                                        
*                                                                               
EDT12B   MVC   PUSER2,WORK                                                      
         MVC   PRDDSC2,WORK        CLEAR OR RE-TRANSMIT FIELD                   
         OI    PRDDSC2H+6,X'80'                                                 
         DROP  R4                                                               
*                                                                               
         OC    ELEM+2(L'PUSER1+L'PUSER2),ELEM+2                                 
         BZ    CKUSRFX                                                          
         MVC   ELEM(2),=X'0832'    ELEMENT CODE/LENGTH                          
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PPRDREC),ELEM,0(R6)                               
*                                                                               
CKUSRFX  DS    0H                  END OF USER FIELDS VALIDATION                
*                                                                               
EDTPST   LA    R2,PRDPSTH                                                       
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   EDT16                                                            
         CLI   5(R2),0                                                          
         BE    EDT16                                                            
         BAS   RE,VALPST                                                        
         BNE   INVERR                                                           
*                                                                               
EDT16    DS    0H                                                               
         LA    R2,PRDADJSH                                                      
         MVC   SAVADJ,PPRDEXCL     SAVE 'OLD' ADJACENCY CODES                   
*                                                                               
         XC    PPRDEXCL,PPRDEXCL                                                
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         CLI   5(R2),3             MAX IS 3                                     
         BH    INVERR                                                           
*                                                                               
         MVC   PPRDEXCL,PRDADJS                                                 
*                                                                               
* CHECK FOR DUPLICATES                                                          
*                                                                               
         CLC   PPRDEXCL(1),PPRDEXCL+1                                           
         BE    DUPERR                                                           
         CLC   PPRDEXCL(1),PPRDEXCL+2                                           
         BE    DUPERR                                                           
         OC    PPRDEXCL+1(2),PPRDEXCL+1                                         
         BZ    EDT20                                                            
         CLC   PPRDEXCL+1(1),PPRDEXCL+2                                         
         BNE   EDT20                                                            
*                                                                               
DUPERR   LA    R3,DUPEDATA                                                      
         B     ERROR                                                            
*                                                                               
EDT20    DS    0H                                                               
*                                                                               
         MVI   PPRDEXC,0                                                        
         LA    R2,PRDEXCLH         EXCLUSION CLASS FIELD                        
         CLI   5(R2),0                                                          
         BE    EDT30                                                            
*                                                                               
         CLI   5(R2),9             MAX IS 9                                     
         BH    INVERR                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         LA    R1,PRDEXCL          POINT TO INPUT FIELD                         
*                                                                               
FINDEXCL LA    RF,EXCLTAB                                                       
CKEXCL   CLC   0(1,R1),0(RF)                                                    
         BE    MOVEEXCL                                                         
         CLC   2(2,RF),=X'0000'                                                 
         BE    INVERR              NOT IN TABLE                                 
         LA    RF,2(RF)                                                         
         B     CKEXCL                                                           
*                                                                               
MOVEEXCL OC    PPRDEXC,1(RF)       "OR" BIT INTO RECORD                         
         AHI   RE,-2                                                            
         LTR   RE,RE                                                            
         BNP   EDT30                                                            
         CLI   1(R1),C','                                                       
         BNE   INVERR              INVALID FORMAT IN INPUT                      
         LA    R1,2(R1)                                                         
         B     FINDEXCL                                                         
*                                                                               
*                B   W   L   T   C                                              
EXCLTAB  DC    X'C280E640D320E310C3080000'                                      
*                                                                               
EDT30    DS    0H                                                               
*                                                                               
* VALIDATING "TRAFFIC?" FIELD                                                   
*                                                                               
         LA    R2,PRDNOTRH                                                      
         NI    PPRDSTAT,X'FF'-X'20'                                             
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    EDT40                                                            
         CLI   8(R2),C'Y'          VALUE IS "Y"?                                
         BE    EDT40                                                            
         CLI   8(R2),C'N'          VALUE IS "N"?                                
         BNE   INVERR                                                           
         OI    PPRDSTAT,X'20'      SET BIT ON, TRAFFIC? = N                     
*                                                                               
EDT40    DS    0H                                                               
         BRAS  RE,CKINFC           CHECKING INTERFACE CODE                      
         BNE   MISSERR                                                          
*                                                                               
EDT50    DS    0H                  FOR FUTURE USE                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   BACT,X'01'          RECORD ACTION ADD?                           
         BNE   PRDCHG              NO                                           
         BAS   RE,ADDREC                                                        
         MVC   PRDADDR,KEY         AFTER ADDREC, KEY HAS ADDR                   
         B     ADDPTRS                                                          
*                                                                               
PRDCHG   BAS   RE,PUTREC                                                        
*                                                                               
* ADD PASSIVE POINTERS HERE                                                     
*                                                                               
ADDPTRS  DS    0H                                                               
         MVC   SAVDMIN,DMINBTS     SAVE 'REAL' DMINBTS                          
         MVC   SAVDMOUT,DMOUTBTS   AND  'REAL  DMOUTBTS                         
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERRORS                      
*                                                                               
         CLC   SAVADJ,PPRDEXCL     SEE IF CODES CHANGED                         
         BE    ADDPX                                                            
         OC    SAVADJ,SAVADJ       SEE IF I HAD OLD                             
         BZ    ADDP50                                                           
         LA    R4,3                                                             
         LA    R6,SAVADJ                                                        
ADDP5    CLI   0(R6),0                                                          
         BE    ADDP20                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(7),PPRDKEY                                                   
         MVI   KEY+3,X'A6'                                                      
         MVC   KEY+7(1),0(R6)                                                   
         MVC   KEY+8(3),PPRDKPRD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PASSIVE POINTER MUST BE FOUND                
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
*                                                                               
ADDP20   LA    R6,1(R6)                                                         
         BCT   R4,ADDP5                                                         
*                                                                               
ADDP50   OC    PPRDEXCL,PPRDEXCL   SEE IF I HAVE ANY NEW                        
         BZ    ADDPX                                                            
         LA    R4,3                                                             
         LA    R6,PPRDEXCL                                                      
ADDP52   CLI   0(R6),0                                                          
         BE    ADDP70                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(7),PPRDKEY                                                   
         MVI   KEY+3,X'A6'                                                      
         MVC   KEY+7(1),0(R6)                                                   
         MVC   KEY+8(3),PPRDKPRD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    ADDP58                                                           
*                                  IF NOT FOUND - THEN ADD POINTER              
         XC    KEY,KEY                                                          
         MVC   KEY(25),KEYSAVE                                                  
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 ADD                                                              
         B     ADDP70                                                           
*                                                                               
ADDP58   MVI   KEY+25,0            UNDELETE POINTER                             
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 WRITE                                                            
*                                                                               
ADDP70   LA    R6,1(R6)                                                         
         BCT   R4,ADDP52           NEXT ADJACENCY CODE                          
*                                                                               
ADDPX    DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDOTPP  DS    0H                  ADD OFFICE/TRAFFIC PASSIVE POINTERS          
         MVC   BYTE,SVPOFFCD       OLD PRD OFFICE CODE                          
         MVC   BYTE2,PPRDOFFC      NEW PRD OFFICE CODE                          
         MVI   BYTE3,POFPKIDQ      PRD OFFICE PPOINTER REC CODE, X'A3'          
         BAS   RE,ADDOTP50                                                      
*                                                                               
         MVC   BYTE,SVPTRACD       OLD PRD TRAFFIC CODE                         
         MVC   BYTE2,PPRDTRAF      NEW PRD TRAFFIC CODE                         
         MVI   BYTE3,PTRPKIDQ      PRD TRAFFIC PPOINTER REC CODE, X'A4'         
         BAS   RE,ADDOTP50                                                      
*                                                                               
         B     ADDOTPX                                                          
*                                                                               
ADDOTP50 DS    0H                                                               
         ST    RE,SVRE                                                          
         CLC   BYTE,BYTE2          OFFICE/TRAFFICE CODES CHANGED?               
         BE    ADDOTP58            NO, DON'T BOTHER WITH PPOINTER               
         CLI   BYTE,0              OLD CODE IS PRESENT?                         
         BE    ADDOTP52            NO, CHECK NEW CODE                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY      AGENCY AND MEDIA                             
         MVC   KEY+3(1),BYTE3      PASSIVE POINTER REC CODE                     
         MVC   KEY+4(3),PPRDKEY+4  CLIENT                                       
         MVC   KEY+7(1),BYTE       OLD OFFICE OR OLD TRAFFICE CODE              
         MVC   KEY+8(3),PPRDKEY+7  PRODUCT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PASSIVE POINTER MUST BE FOUND                
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
*                                                                               
ADDOTP52 CLI   BYTE2,0             SEE IF NEW CODE IS PRESENT                   
         BE    ADDOTP58                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY      AGENCY AND MEDIA                             
         MVC   KEY+3(1),BYTE3      PASSIVE POINTER REC CODE                     
         MVC   KEY+4(3),PPRDKEY+4  CLIENT                                       
         MVC   KEY+7(1),BYTE2      NEW OFFICE OR NEW TRAFFICE CODE              
         MVC   KEY+8(3),PPRDKEY+7  PRODUCT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     ALREADY EXIST IN DIRECTORY?                  
         BE    ADDOTP56            YES                                          
*                                                                               
         XC    KEY,KEY             IF NOT FOUND, ADD PPOINTER                   
         MVC   KEY(25),KEYSAVE                                                  
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 ADD                                                              
         B     ADDOTP58                                                         
*                                                                               
ADDOTP56 MVI   KEY+25,0            UNDELETE POINTER                             
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 WRITE                                                            
*                                                                               
ADDOTP58 L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
ADDOTPX  DS    0H                  END OF OFFICE AND TRAFFIC PPOINTERS          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVC   DMINBTS,SAVDMIN     RESTORE DMINBTS AND DMOUTBTS                 
         MVC   DMOUTBTS,SAVDMOUT                                                
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE PST CODES                                                            
*                                                                               
VALPST   NTR1                                                                   
         LA    R6,PPRDELEM                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   VP10                                                             
*                                                                               
* CHANGE - DELETE OLD ELEM                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PPRDREC),0(R6)                                    
*                                                                               
VP10     LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,PRDPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VTWA        A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB       GET PST ADDRESS                              
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   PSTERR,0                                                         
         BNE   VPNO                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'250C'                                                 
         MVC   ELEM+2(10),PSTOUT                                                
         GOTO1 VRECUP,DMCB,(1,PPRDREC),ELEM,0(R6)                               
         BAS   RE,DISPPST                                                       
*                                                                               
VPYES    SR    R1,R1                                                            
VPNO     LTR   R1,R1                                                            
VPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY PST CODES                                                             
*                                                                               
DISPPST  NTR1                                                                   
         MVC   PRDPST,SPACES       OUTPUT                                       
         OI    PRDPSTH+6,X'80'                                                  
         LA    R6,PPRDELEM                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL           ANYTHING TO DISPLAY                          
         BNE   DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VTWA        A(COMFACS)                                   
         XC    DMCB(12),DMCB       GET PST ADDRESS                              
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   PRDPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SET USER DESCRIPTION FIELDS ON THE SCREEN                                     
*                                                                               
SETUSER  NTR                                                                    
         MVC   MYAREA(L'SVP1USER),SVP1USER                                      
         LA    R1,PRDDEF1H                                                      
         LA    R6,PRDDSC1H                                                      
         BAS   RE,FMTUSR           DISPLAY 1ST DESC LINE                        
*                                                                               
         MVC   MYAREA(L'SVP2USER),SVP2USER                                      
         LA    R1,PRDDEF2H                                                      
         LA    R6,PRDDSC2H                                                      
         BAS   RE,FMTUSR           DISPLAY 2ND DESC LINE                        
*                                                                               
SUX      XIT                                                                    
         EJECT                                                                  
*                                                                               
* PUT OUT USER DESCRIPTION FIELDS                                               
*       R1   = A(DESC FIELD)                                                    
*       R6   = A(INPUT FIELD)                                                   
*       DESC = PRODUCT DESCRIPTION                                              
*                                                                               
FMTUSR   DS    0H                                                               
         OI    1(R6),X'20'         PROTECT INPUT FIELD                          
*                                                                               
         MVC   8(L'SVP1USER,R1),MYAREA                                          
         CLC   SPACES(L'SVP1USER),MYAREA                                        
         BL    FMTUSR10                                                         
         LR    R0,R1               SAVE C(R1) AROUND.                           
         ZIC   R1,0(R6)            R1=L(HEADER)+L(INPUT FIELD)                  
         AHI   R1,-8               R1=L(INPUT FIELD)                            
         TM    1(R6),X'02'         CHECK FOR EXTENDED HEADER                    
         BZ    *+8                                                              
         AHI   R1,-8               SUBTRACT L(X-HEADER)                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)       CLEAR ANY GARBAGE                            
         LR    R1,R0                                                            
         B     FMTX                                                             
*                                                                               
FMTUSR10 DS    0H                                                               
         TM    12(RA),X'08'        AUTHORIZED?                                  
         BO    *+8                                                              
         NI    1(R6),X'FF'-X'20'   UNPROTECT INPUT FIELD                        
*                                                                               
FMTX     OI    6(R1),X'80'         TRANSMIT FIELD                               
         OI    6(R6),X'80'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INPUT:                                                                        
*        R2 = A(INPUT FIELD)                                                    
*        R3 = USER BLOCK FROM CLIENT RECORD                                     
*                                                                               
* OUTPUT:                                                                       
*        WORK = DATA OR NULLS                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTUSR   DS    0H                                                               
         USING UDEFD,R3                                                         
         ST    RE,SVRE                                                          
         ST    R4,SVR4                                                          
         XC    WORK,WORK                                                        
         OC    0(L'SVP1USER,R3),0(R3)                                           
         BZ    XEDTUSR                                                          
         CLI   5(R2),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR5                                                          
         TM    UFLG1,X'80'         IS INPUT REQUIRED                            
         BNO   XEDTUSR                                                          
         B     MISSERR                                                          
*                                                                               
EDTUSR5  CLC   5(1,R2),ULEN        CHECK IF L'INPUT IS VALID                    
         BH    LONGERR             NOT VALID                                    
         CLI   UTYPE,C' '          ACCEPT ANY INPUT                             
         BNH   EDTUSR30                                                         
         CLI   UTYPE,C'N'          IF TYPE IS NUMERIC                           
         BNE   EDTUSR10                                                         
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    EDTUSR30                                                         
         ZIC   R1,5(R2)            BUT ALLOW '- /'                              
         LA    R4,8(R2)                                                         
*                                                                               
EDTUSR7  CLI   0(R4),C'0'                                                       
         BL    EDTUSR8                                                          
         CLI   0(R4),C'9'                                                       
         BNH   EDTUSR9                                                          
*                                                                               
EDTUSR8  CLI   0(R4),C' '                                                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'/'                                                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'-'                                                       
         BNE   INVERR                                                           
*                                                                               
EDTUSR9  LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR7                                                       
         B     EDTUSR30                                                         
*                                                                               
EDTUSR10 CLI   UTYPE,C'C'          IF TYPE IS ALPHABETIC                        
         BNE   EDTUSR20                                                         
         ZIC   R1,5(R2)            ALLOW ALL INPUT EXCEPT NUMBERS               
         LA    R4,8(R2)                                                         
*                                                                               
EDTUSR15 CLI   0(R4),C'0'                                                       
         BL    EDTUSR17                                                         
         CLI   0(R4),C'9'                                                       
         BNH   INVERR                                                           
*                                                                               
EDTUSR17 LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR15                                                      
         B     EDTUSR30                                                         
*                                                                               
EDTUSR20 CLI   UTYPE,C'D'          IF TYPE DATE                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATVAL,DMCB,(0,8(R2)),TDATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         L     R4,0(R1)            L'DATE                                       
         ZIC   R1,5(R2)            L'INPUT                                      
         SR    R1,R4                                                            
         BNZ   INVERR                                                           
*                                                                               
EDTUSR30 ZIC   R1,5(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       MOVE INPUT INTO WORK                         
*                                                                               
XEDTUSR  L     RE,SVRE                                                          
         L     R4,SVR4                                                          
         BR    RE                                                               
*                                                                               
DTERR    LA    R3,20               INVALID DATE                                 
         B     ERROR                                                            
*                                                                               
INVERR   LA    R3,FLDINV           INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
MISSERR  LA    R3,1                MISSING INPUT                                
         B     ERROR                                                            
*                                                                               
LONGERR  LA    R3,32               INPUT IS TOO LONG                            
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FORMATP  DS    0H                                                               
*                                                                               
PUTFLD   FOUT  PRDPRDNH,PPRDNAME,20                                             
*                                                                               
         XC    PRDADJS,PRDADJS                                                  
         XC    PRDEXCL,PRDEXCL                                                  
*                                                                               
         FOUT  PRDADJSH,PPRDEXCL,3                                              
         FOUT  PRDOANH,PPRDOAN,2                                                
         FOUT  PRDBRNH,PPRDBILL,20                                              
         FOUT  PRDBRN2H,PPRDBIL2,20                                             
         FOUT  PRDAL1H,PPRDLIN1,30                                              
         FOUT  PRDAL2H,PPRDLIN2,30                                              
         FOUT  PRDATTNH,PPRDATTN,24                                             
*                                                                               
******** BRAS  RE,PRDSECUR         PRODUCT LEVEL SECURITY                       
*                                                                               
         BRAS  RE,LWROTAT          DISPLAY LEGAL WARNING ROTATION               
*                                                                               
         BRAS  RE,INFCODE          DISPLAY INTERFACE CODE                       
*                                                                               
         BRAS  RE,NOTRAFF          DISPLAY TRAFFIC = Y/N FIELD                  
*                                                                               
         BAS   RE,PUTEXCL          DISPLAY EXCLUSION CLASS FIELDS               
*                                                                               
         BAS   RE,PUTUSER          DISPLAY USER DEFINITION FIELDS               
*                                                                               
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   NOCANAD                                                          
         BAS   RE,DISPPST          DISPLAY PST CODES                            
         FOUT  PRDGSTH,PPRDGST,1                                                
*                                                                               
NOCANAD  CLC   PPRDOAN,=C'   '                                                  
         BNH   NOAOR                                                            
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY  AGENCY + MEDIA                                   
         MVI   KEY+3,X'16'     ID                                               
         MVC   KEY+4(2),PRDOAN                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BE    YYOVEOK                                                          
         FOUT  HDRMSGH,=C'*** OAN CODE NONEXISTANT *****',30                    
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     ZIPOUT                                                           
*                                                                               
YYOVEOK  DS    0H                                                               
         GOTO1 GET2ZIO                                                          
         LA    R2,PRDOANN                                                       
         LA    R3,ZZZIO                                                         
         USING POTHRECD,R3                                                      
         FOUT  PRDOANNH,POTHNAME,33                                             
         MVC   KEY,SAVEKEY         PRODUCT HEADER                               
         MVC   OANMSG+21(2),PPRDOAN                                             
         MVC   HDRPRDN+20(L'OANMSG),OANMSG                                      
         NI    HDRPRDNH+7,X'80'                                                 
         OI    HDRPRDNH+7,X'50'    NEW LENGTH OF OUTPUT                         
         GOTO1 HIGH                RESET DIRECTORY                              
         GOTO1 GET2ZIO             READ PRD HDR INTO ZZZIO TO PRIME D/M         
*                                                                               
NOAOR    XC    PRDACCT,PRDACCT                                                  
         CLI   PPRDACCT,X'FF'                                                   
         BNE   PUTACCT                                                          
         MVI   PPRDACCT,0                                                       
         MVC   FULL,PPRDACCT                                                    
         L     R0,FULL                                                          
FLDINV   EQU   2                                                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRDACCT(5),DUB+5(3)                                              
         FOUT  PRDACCTH                                                         
         B     CKPROF                                                           
*                                                                               
PUTACCT  FOUT  PRDACCTH,PPRDACCT,4                                              
CKPROF   CLI   CPROFLE,C'0'                                                     
         BE    PUTFLDX                                                          
*                                                                               
PUTFLD1  FOUT  PRDDIVH,PPRDDIV,3                                                
         BAS   R8,PUTDIV                                                        
PUTFLDX  CLI   BACT,X'02'          IF ACTION =CHANGE - DONE                     
         BNE   DONE                                                             
NOTDONE  LA    R2,PRDPRDNH                                                      
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT GEN                 READ OTHERAGENCY NAME INTO ZZZIO             
GET2ZIO  NTR                                                                    
         PRINT NOGEN                                                            
         MVC   COMMAND,=C'GETREC'                                               
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),ZZZIO,(TERMNAL,DMWORK)                                      
         B     DMCHECK                                                          
*                                                                               
* FIND UDEF ELEMENT & DISPLAY INFO                                              
*                                                                               
PUTUSER  NTR1                                                                   
         MVC   PRDDSC1,SPACES      CLEAR PREVIOUS INFO                          
         MVC   PRDDSC2,SPACES                                                   
         OI    PRDDSC1H+6,X'80'    TRANSMIT                                     
         OI    PRDDSC2H+6,X'80'                                                 
                                                                                
         LA    R6,PPRDELEM                                                      
         USING PPRDUDEF,R6                                                      
         MVI   ELCODE,X'08'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PUX                                                              
*                                                                               
         OC    SVP1USER,SVP1USER                                                
         BZ    *+10                                                             
         MVC   PRDDSC1,PUSER1                                                   
*                                                                               
         OC    SVP2USER,SVP2USER                                                
         BZ    *+10                                                             
         MVC   PRDDSC2,PUSER2                                                   
*                                                                               
         OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
*                                                                               
PUX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTEXCL  NTR1                      DISPLAY EXCLUSION CLASS FIELD                
         LA    R1,PRDEXCL                                                       
         LA    R4,EXCLTAB1                                                      
CKBITS   MVC   BYTE,PPRDEXC                                                     
         NC    PPRDEXC,0(R4)                                                    
         CLC   PPRDEXC,BYTE                                                     
         BE    NEXTBT                                                           
         MVC   0(1,R1),1(R4)                                                    
         OC    PPRDEXC,PPRDEXC                                                  
         BZ    PUTEXCL9                                                         
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)                                                         
*                                                                               
NEXTBT   CLC   2(2,R4),=X'0000'                                                 
         BE    PUTEXCL9                                                         
         LA    R4,2(R4)                                                         
         OC    PPRDEXC,PPRDEXC                                                  
         BZ    PUTEXCL9                                                         
         B     CKBITS                                                           
*                  B   W   L   T   C                                            
EXCLTAB1 DC    X'7FC2BFE6DFD3EFE3F7C30000'                                      
*                                                                               
PUTEXCL9 OI    PRDEXCLH+6,X'80'    TRANSMITT TO SCREEN                          
*                                                                               
PUTEXCLX B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
DELETE   EQU   *                                                                
         CLI   BACT,1              SEE IF ADD                                   
         BNE   DEL5                                                             
         LA    R3,FLDINV           NO DELETE ON ADD                             
         B     ERROR                                                            
*                                                                               
DEL5     LA    R4,PRDTAB1          TABLE OF REC TYPES I CAN'T FIND              
DEL6     CLI   0(R4),0                                                          
         BE    DEL10                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVC   KEY+3(1),0(R4)                                                   
         GOTO1 HIGH                                                             
         B     DEL6S5                                                           
*                                                                               
DEL6S    GOTO1 SEQ                                                              
DEL6S5   CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL8                                                             
         CLI   KEY+3,X'08'         SEE IF BILL WITH EST = 000                   
         BNE   DEL7                THESE ARE OLD "ESTIMATE SUMMARY"             
         OC    KEY+10(2),KEY+10                                                 
         BZ    DEL6S                                                            
DEL7     LA    R3,DELERR                                                        
         B     ERROR                                                            
DEL8     EQU   *                                                                
         LA    R4,1(R4)                                                         
         B     DEL6                                                             
*                                                                               
DEL10    LA    R4,PRDTAB2          TABLE OF REC TYPES TO DELETE                 
DEL11    CLI   0(R4),0                                                          
         BE    DEL20                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVC   KEY+3(1),0(R4)                                                   
         GOTO1 HIGH                                                             
         B     DEL15                                                            
*                                                                               
DEL13    GOTO1 SEQ                                                              
DEL15    EQU   *                                                                
         CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL18                                                            
         CLI   KEY+3,X'08'         SEE IF BILL                                  
         BNE   DEL16                                                            
         OC    KEY+10(2),KEY+10    WITH EST =0                                  
         BZ    DEL16                                                            
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                  SINCE THIS BILL SHOULD HAVE CAUSED           
*                                  DELERR IN DEL6 LOGIC                         
*                                                                               
DEL16    OI    KEY+25,X'80'        DELETE POINTER                               
         GOTO1 WRITE                                                            
         B     DEL13                                                            
*                                                                               
DEL18    LA    R4,1(R4)                                                         
         B     DEL11                                                            
*                                                                               
DEL20    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(7),PPRDKEY                                                   
         MVI   KEY+3,X'10'         CHK FOR PRD CONTRACTS                        
         GOTO1 HIGH                                                             
         B     DEL23                                                            
*                                                                               
DEL22    GOTO1 SEQ                                                              
DEL23    EQU   *                                                                
         CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL30                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',KEY+27,  X        
               ZZZIO,(TERMNAL,DMWORK)                                           
*                                                                               
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R7,ZZZIO                                                         
         USING PCONREC,R7                                                       
         CLC   PCONPRD,PPRDKPRD                                                 
         BNE   DEL22                                                            
         OI    PCONCTRL,X'80'                                                   
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'PUTREC'),=C'PRTFILE',KEY+27,  X        
               ZZZIO,(TERMNAL,DMWORK)                                           
*                                                                               
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+25,X'80'        DELETE POINTER                               
         GOTO1 WRITE                                                            
         B     DEL22                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
DEL30    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVI   KEY+3,X'3B'         CHK FOR PRD GROUP DIR REC                    
         GOTO1 HIGH                                                             
         B     DEL33                                                            
*                                                                               
DEL32    GOTO1 SEQ                                                              
DEL33    EQU   *                                                                
         CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL40                                                            
         OI    KEY+25,X'80'        DELETE '3B' PRD GRP DIR REC                  
         GOTO1 WRITE                                                            
         B     DEL32                                                            
*                                                                               
DEL40    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(7),PPRDKEY                                                   
         MVI   KEY+3,X'3E'         CHK FOR PRD GROUP DIR REC                    
         GOTO1 HIGH                                                             
         B     DEL43                                                            
*                                                                               
DEL42    GOTO1 SEQ                                                              
DEL43    EQU   *                                                                
         CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL50                                                            
         LA    R7,KEY                                                           
         USING GRPGKEY,R7                                                       
         CLC   PPRDKPRD,GRPGVAL    CK THE PRODUCT CODE                          
         BNE   DEL42                                                            
         OI    GRPGCNTL,X'80'      DELETE '3E' PRD GRP DIR REC                  
         GOTO1 WRITE                                                            
         B     DEL42                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
DEL50    DS    0H                  PASSIVE POINTER DELETIONS                    
         CLI   PPRDOFFC,0                                                       
         BE    *+18                NO PRD OFFICE PPOINTER TO DELETE             
         MVC   BYTE,PPRDOFFC                                                    
         MVI   BYTE3,X'A3'                                                      
         BAS   RE,DEL50PP          DELETE PRD OFFICE PASSIVE POINTER            
*                                                                               
         CLI   PPRDTRAF,0                                                       
         BE    *+18                NO PRD TRAFFIC PPOINTER TO DELETE            
         MVC   BYTE,PPRDTRAF                                                    
         MVI   BYTE3,X'A4'                                                      
         BAS   RE,DEL50PP          DELETE PRD TRAFFIC PASSIVE POINTER           
*                                                                               
         B     DEL60               DONE WITH OFFICE AND TRAFFIC PPTRS           
*                                                                               
DEL50PP  ST    RE,SVRE                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY      AGENCY AND MEDIA                             
         MVC   KEY+3(1),BYTE3      PASSIVE POINTER REC CODE                     
         MVC   KEY+4(3),PPRDKEY+4  CLIENT                                       
         MVC   KEY+7(1),BYTE       OFFICE OR TRAFFICE CODE                      
         MVC   KEY+8(3),PPRDKEY+7  PRODUCT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     PASSIVE POINTER FOUND?                       
         BNE   DEL50PPX            NO                                           
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
DEL50PPX L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
DEL60    DS    0H                  FUTURE USES                                  
*                                                                               
* NOW DELETE PRODUCT RECORD                                                     
*                                                                               
DEL70    MVC   KEY+27(4),PRDADDR                                                
         BAS   RE,GETREC                                                        
         OI    PPRDCNTL,X'80'                                                   
         GOTO1 PUTREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         FOUT  HDRMSGH,=C'** PRODUCT DELETED **',21                             
ZIPOUT   LA    R2,HDRRECH                                                       
         OI    6(R2),OI1C          SET CURSOR                                   
*                                                                               
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
DIVREAD  XC    PRDDIVN,PRDDIVN                                                  
         FOUT  PRDDIVNH                                                         
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
         MVC   KMED,HDRMED                                                      
         MVC   KCLT,HDRCLT                                                      
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+7(3),PPRDDIV                                                 
         XC    KEY+10(10),KEY+10                                                
*                                                                               
         BAS   RE,READ                                                          
         MVC   DMWORK1(96),DMWORK                                               
         LA    R0,IOAREA+1000      SET 'TO' ADDRESS                             
         LA    R1,1000             SET 'TO' LENGTH                              
         LA    RE,IOAREA           SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
         BAS   RE,GETREC                                                        
         MVC   DMWORK(96),DMWORK1                                               
         FOUT  PRDDIVNH,PDIVNAME,20                                             
         FOUT  PRDDIVH,PDIVKDIV,3                                               
         MVC   KEY(32),SAVEKEY                                                  
         LA    R0,IOAREA           SET 'TO' ADDRESS                             
         LA    R1,1000             SET 'TO' LENGTH                              
         LA    RE,IOAREA+1000      SET 'FROM' ADDRESS                           
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
         BR    R8                                                               
*                                                                               
PUTDIV   CLI   CPROFLE,C'1'                                                     
         BE    PUTDIV1                                                          
         CLI   CPROFLE,C'2'                                                     
         BE    PUTDIV1                                                          
         MVC   PRDDIVN(20),=CL20'* DIVS NOT ALLOWED *'                          
         FOUT  PRDDIVNH                                                         
         BR    R8                                                               
PUTDIV1  XC    PRDDIVN,PRDDIVN                                                  
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+7(3),PPRDDIV                                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(25),KEY                                                  
         BE    PUTDIV2                                                          
         MVC   PRDDIVN(20),=CL20'* DIV NOT ON FILE *'                           
         FOUT  PRDDIVNH                                                         
         BR    R8                                                               
PUTDIV2  BAS   RE,GETREC                                                        
         FOUT  PRDDIVNH,PDIVNAME,20                                             
         BR    R8                                                               
*                                                                               
DONE     MVI   DONESW,1                                                         
         BRAS  RE,SETUPSCR         CERTAIN FIELDS NEED TO BE PROTECTED          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRDTAB1  DC    X'0708201400'       ESTS,INVS,BUYS,AOR                           
PRDTAB2  DC    X'08151800'         BILLS (NO EST), ADS,BUDGETS                  
*                                                                               
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL80' '                                                          
ELCODE   DS    XL1                                                              
MYAREA   DS    CL24                                                             
PSTOUT   DS    CL64                                                             
SVRE     DS    F                                                                
SVR4     DS    F                                                                
TDATE    DS    CL6                                                              
SAVEKEY  DS    CL32                                                             
DMWORK1  DS    12D                                                              
*                                                                               
SAVADJ   DS    CL3                 OLD ADJACENCY CODES                          
SAVDMIN  DS    CL1                                                              
SAVDMOUT DS    CL1                                                              
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
RELO     DS    F                                                                
*                                                                               
WKKEY    DS    CL32                GENERAL WORKING STORAGE KEY                  
*                                                                               
MSSNGERR EQU   1                                                                
DIVERR1  EQU   91                                                               
DIVERR2  EQU   92                                                               
DIVERR3  EQU   93                                                               
DELERR   EQU   198                                                              
DUPEDATA EQU   170                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETUPSCR NTR1  BASE=*,LABEL=*      SETUP PROTECTED FIELDS                       
*                                                                               
         CLC   PPRDKAGY(2),=C'H9'  STARCOM?                                     
         BE    SETSCR30                                                         
         CLC   PPRDKAGY(2),=C'SJ'                                               
         BE    SETSCR30                                                         
         XC    PRDLWT,PRDLWT                                                    
         OI    PRDLWTH+6,X'80'     TRANSMIT                                     
         XC    PRDLWRO,PRDLWRO                                                  
         OI    PRDLWROH+1,X'20'    PROTECT FIELD                                
         OI    PRDLWROH+6,X'80'    TRANSMIT                                     
         B     SETSCRX                                                          
*                                                                               
SETSCR30 TM    12(RA),X'08'        AUTHORIZED?                                  
         BO    SETSCR50                                                         
*                                                                               
         LA    R2,PRDLWROH                                                      
         OI    6(R2),X'20'         CHG TO PROTECTED FOR NEXT INPUT              
         B     SETSCRX                                                          
*                                                                               
SETSCR50 LA    R2,PRDFRSTH         FIRST FIELD ON PRD SCR                       
         LA    R3,PRDENDX          LAST FIELD ON PRD SCR                        
*                                                                               
SETSCR60 CR    R2,R3               LAST SCREEN FIELD YET?                       
         BH    SETSCR80            YES                                          
         TM    6(R2),X'20'         ALREADY CHGED TO PROTECTED?                  
         BO    *+8                                                              
         OI    6(R2),X'20'         CHG TO PROTECTED FOR NEXT INPUT              
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     SETSCR60            DONE NEXT FIELD ON SCREEN                    
*                                                                               
SETSCR80 LA    R2,PRDLWROH         NEED TO UNPROTECT LW ROTATION FLD            
         NI    6(R2),X'FF'-X'20'                                                
*                                                                               
SETSCRX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
SETSCRER LTR   RB,RB               NOT EQUAL (ERROR, INVALID NUMBER)            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKOFFTRA NTR1  BASE=*,LABEL=*      CHECKING PRD OFFICE AND TRAFFIC              
*                                                                               
         MVI   SVPOFFCD,0                                                       
         MVI   SVPTRACD,0                                                       
         TM    PRDOFFH+1,X'20'     PROTECTED?                                   
         BO    CKOTX               NO NEED TO CHECK ANYMORE                     
         MVC   PRDOFFX,SPACES      CLEAR HEX DISPLAYS IF ANY                    
         OI    PRDOFFXH+6,X'80'                                                 
         MVC   PRDTRAX,SPACES                                                   
         OI    PRDTRAXH+6,X'80'                                                 
         MVC   SVPOFFCD,PPRDOFFC   SAVE ORIGINAL PRD OFFICE CODE                
         MVC   SVPTRACD,PPRDTRAF   SAVE ORIGINAL PRD TRAFFICE CODE              
         XC    PPRDOFFC(3),PPRDOFFC                                             
*                                                                               
* CODES TO CHECK FOR PRD PROF GO HERE (REQUIRED FLD OR NOT)                     
*                                                                               
         LA    R2,PRDOFFH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKOT30                                                           
         BAS   RE,CKOT80           CHK FOR SPECIAL VALID CHARS                  
         MVC   BYTE,PRDOFF                                                      
         LA    R5,PRDOFFXH         FOR DISPLAYING HEX VALUES                    
         BAS   RE,CKOTDH                                                        
         MVC   PPRDOFFC,PRDOFF     OFFICE CODE HAS BEEN VALIDATED               
*                                                                               
CKOT30   LA    R2,PRDTRAH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKOT40                                                           
         BAS   RE,CKOT80           CHK FOR SPECIAL VALID CHARS                  
         MVC   BYTE,PRDTRA                                                      
         LA    R5,PRDTRAXH         FOR DISPLAYING HEX VALUES                    
         BAS   RE,CKOTDH                                                        
         MVC   PPRDTRAF,PRDTRA     TRAFFIC CODE HAS BEEN VALIDATED              
*                                                                               
CKOT40   DS    0H                  FUTURE USES                                  
         B     CKOTX                                                            
*                                                                               
CKOT80   CLI   8(R2),C'='          CHK FOR INVALID CHARS FOR PRD                
         BE    CKOTERR             OFFICE AND TRAFFIC CODES                     
         CLI   8(R2),C'-'                                                       
         BE    CKOTERR                                                          
         CLI   8(R2),C','                                                       
         BE    CKOTERR                                                          
         CLI   8(R2),C'.'                                                       
         BE    CKOTERR                                                          
         BR    RE                                                               
*                                                                               
CKOTX    CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKOTERR  LTR   RB,RB               NOT EQUAL (ERROR, INVALID NUMBER)            
         XIT1  REGS=(R2)           CURSOR POSITION FOR ERROR                    
*                                                                               
CKOTDH   ST    RE,FULL             HEX PRD OFFICE/TRAFFIC DISP                  
         CLI   BYTE,X'40'          SHOW HEX VALUE?                              
         BNH   CKOTDHX             NO                                           
         CLI   BYTE,X'D0'          SHOW HEX VALUE?                              
         BE    CKOTDH50            YES                                          
         CLI   BYTE,X'E0'          SHOW HEX VALUE?                              
         BE    CKOTDH50            YES                                          
         CLI   BYTE,C'A'           SHOW HEX VALUE?                              
         BL    CKOTDH50            YES                                          
         CLI   BYTE,C'9'           SHOW HEX VALUE?                              
         BNH   CKOTDHX             NO                                           
*                                                                               
CKOTDH50 GOTO1 =V(HEXOUT),DMCB,BYTE,8(R5),1,RR=RELO                             
         OI    6(R5),X'80'                                                      
*                                                                               
CKOTDHX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKLWRO   NTR1  BASE=*,LABEL=*      CHECK LEGAL WARNING ROTATION ORDERS          
*                                                                               
         LA    R2,PRDLWROH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKLWRO40            NO, CK IF OLD ELEM NEEDS TO DELETE           
*                                                                               
         CLI   5(R2),11            EXACTLY 11 CHARS INPUT?                      
         BNE   CKLWROER            NO, ERROR                                    
*                                                                               
         XC    SVLWRO,SVLWRO                                                    
*                                                                               
         CLC   8(11,R2),=C'A1,B2,C3,D4'                                         
         BNE   *+14                                                             
         MVC   SVLWRO,=C'ABCD'                                                  
         B     CKLWRO40                                                         
         CLC   8(11,R2),=C'B1,C2,D3,A4'                                         
         BNE   *+14                                                             
         MVC   SVLWRO,=C'BCDA'                                                  
         B     CKLWRO40                                                         
         CLC   8(11,R2),=C'C1,D2,A3,B4'                                         
         BNE   *+14                                                             
         MVC   SVLWRO,=C'CDAB'                                                  
         B     CKLWRO40                                                         
         CLC   8(11,R2),=C'D1,A2,B3,C4'                                         
         BNE   *+14                                                             
         MVC   SVLWRO,=C'DABC'                                                  
         B     CKLWRO40                                                         
*                                                                               
         B     CKLWROER            NO OTHER COMBINATIONS YET                    
*                                                                               
CKLWRO40 LA    R6,PPRDREC+33       POINT TO 1ST ELEM (CANNOT BE X'40')          
         MVI   ELCODE,X'40'                                                     
         BAS   RE,LWNXTEL          FIND LW ROTATION ELEM, REMOVE IT             
         BNE   CKLWRO50                                                         
         GOTO1 VRECUP,DMCB,(1,PPRDREC),0(R6)                                    
*                                                                               
         B     CKLWRO40            SHOULD NOT BE ANY MORE                       
*                                                                               
CKLWRO50 DS    0H                                                               
         CLI   5(R2),0             OLD ELEM DELETED, ANY INPUTS?                
         BE    CKLWROX             NO, DONE                                     
*                                                                               
         LA    R4,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PPRDLWEL,R4                                                      
         MVI   0(R4),X'40'         LW ROTATION ELEM CODE                        
         MVI   1(R4),08            LW ROATAION ELEM LENGTH                      
         MVC   PPRDROTA,SVLWRO                                                  
*                                                                               
         BAS   RE,LWCLRREC                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PPRDREC),ELEM,0(R6)                               
         DROP  R4                                                               
*                                                                               
CKLWROX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKLWROER LTR   RB,RB               NOT EQUAL (ERROR, INVALID NUMBER)            
         XIT1  REGS=(R2)           CURSOR POSITION FOR ERROR                    
*                                                                               
SVLWRO   DS    CL4                 ABCD, BCDA, CDAB OR DABC                     
LWFULL   DS    F                                                                
*                                                                               
LWNXTEL  CLI   0(R6),0                                                          
         BE    LWNXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     LWNXTEL                                                          
*                                                                               
LWNXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
LWCLRREC ST    RE,LWFULL           ROUTINE TO CLEAR END OF REC                  
         MVC   HALF,PPRDREC+25                                                  
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         SR    RE,RE                                                            
         LA    RE,PPRDREC                                                       
         AR    RE,R1                                                            
         SR    RF,RF                                                            
         LA    RF,1000             IOAREA IS 1000                               
         SR    RF,RE               NUMBER OF BYTES NEED TO BE CLEARED           
         XCEF                                                                   
         L     RE,LWFULL                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKINFC   NTR1  BASE=*,LABEL=*      CHECK FOR PRODUCT INTERFACE CODE             
*                                                                               
         TM    12(RA),X'08'        TRAFFIC USER AUTHORIZED?                     
         BO    CKINFCX             YES, NO NEED TO CHECK INTERFACE CODE         
*                                                                               
         LA    R2,PRDINFCH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BNE   CKINFC40                                                         
         CLI   F0PROF+5,C'P'       ALLOWED FOR PRODUCT?                         
         BE    CKINFCER            YES, MISSING ERROR                           
         CLI   F0PROF+5,C'B'       ALLOWED FOR BOTH PRODUCT AND CLIENT?         
         BE    CKINFCER            YES, MISSING ERROR                           
*                                                                               
CKINFC40 DS    0H                                                               
         LA    R6,PPRDREC+33       POINT TO 1ST ELEM (CANNOT BE X'30')          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,I1NXTEL          FIND INTERFACE CODE EL, REMOVE IT            
         BNE   CKINFC50                                                         
         GOTO1 VRECUP,DMCB,(1,PPRDREC),0(R6)                                    
*                                                                               
         B     CKINFC40            SHOULD NOT BE ANY MORE                       
*                                                                               
CKINFC50 DS    0H                                                               
         CLI   5(R2),0             OLD ELEM DELETED, ANY INPUTS?                
         BE    CKINFCX             NO, DONE                                     
*                                                                               
         LA    R4,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PPRDICEL,R4                                                      
         MVI   0(R4),X'30'         INTERFACE CODE ELEM CODE                     
         MVI   1(R4),12            INTERFACE CODE ELEM LENGTH                   
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDINFC(0),8(R2)                                                
         OC    PPRDINFC,=5C' '     SPACE PADDED                                 
*                                                                               
         BAS   RE,ICCLRREC                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PPRDREC),ELEM,0(R6)                               
         DROP  R4                                                               
*                                                                               
CKINFCX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKINFCER LTR   RB,RB               NOT EQUAL (ERROR, INVALID NUMBER)            
         XIT1  REGS=(R2)           CURSOR POSITION FOR ERROR                    
*                                                                               
ICFULL   DS    F                                                                
*                                                                               
I1NXTEL  CLI   0(R6),0                                                          
         BE    I1NXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     I1NXTEL                                                          
*                                                                               
I1NXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
ICCLRREC ST    RE,ICFULL           ROUTINE TO CLEAR END OF REC                  
         MVC   HALF,PPRDREC+25                                                  
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         SR    RE,RE                                                            
         LA    RE,PPRDREC                                                       
         AR    RE,R1                                                            
         SR    RF,RF                                                            
         LA    RF,1000             IOAREA IS 1000                               
         SR    RF,RE               NUMBER OF BYTES NEED TO BE CLEARED           
         XCEF                                                                   
         L     RE,ICFULL                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOTRAFF  NTR1  BASE=*,LABEL=*      DISPLAY "TRAFFIC?" Y/N FIELD                 
*                                                                               
         MVI   PRDNOTR,C'Y'        DEFAULT IS TRAFFIC? = Y                      
         OI    PRDNOTRH+6,X'80'                                                 
         LA    R6,PPRDREC+33                                                    
         CLI   0(R6),X'06'         FIRST PRD ELEM PRESENT?                      
         BE    *+6                                                              
         DC    H'0'                FIRST PRD ELEM HAS TO BE THERE               
         USING PPRDELEM,R6                                                      
         TM    PPRDSTAT,X'20'      IS TRAFFIC? = N?                             
         BZ    NOTRAFFX                                                         
         MVI   PRDNOTR,C'N'                                                     
         OI    PRDNOTRH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
NOTRAFFX XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INFCODE  NTR1  BASE=*,LABEL=*      DISPLAY INTERFACE CODE FIELD                 
*                                                                               
         MVC   PRDINFC,=5C' '                                                   
         OI    PRDINFCH+6,X'80'    TRANSMIT CLEARED FLD                         
*                                                                               
         LR    R3,R6               SAVE R6, IN CASE IS USED ELSE WHERE          
         LA    R6,PPRDREC+33                                                    
         MVI   ELCODE,X'30'                                                     
         BAS   RE,I2NXTEL                                                       
         BNE   INFCODEX                                                         
         USING PPRDICEL,R6                                                      
         MVC   PRDINFC,PPRDINFC    PUT ELEM DATA ON SCREEN FLD                  
         OI    PRDINFCH+6,X'80'                                                 
         LR    R6,R3               RESTORE R6                                   
         DROP  R6                                                               
INFCODEX XIT1                                                                   
*                                                                               
I2NXTEL  CLI   0(R6),0                                                          
         BE    I2NXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     I2NXTEL                                                          
*                                                                               
I2NXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LWROTAT  NTR1  BASE=*,LABEL=*      DISPLAY LEGAL WARNING ROTATION               
*                                                                               
         LA    R2,PRDLWROH                                                      
         MVC   8(11,R2),=11C' '                                                 
         OI    6(R2),X'80'         TRANSMIT CLEARED FLD                         
*                                                                               
         LR    R3,R6               SAVE R6, IN CASE IS USED ELSE WHERE          
         LA    R6,PPRDREC+33                                                    
         MVI   ELCODE,X'40'                                                     
         BAS   RE,L2NXTEL                                                       
         BNE   LWROTATX                                                         
*                                                                               
         USING PPRDLWEL,R6                                                      
         CLC   PPRDROTA,=C'ABCD'                                                
         BNE   *+14                                                             
         MVC   8(11,R2),=C'A1,B2,C3,D4'                                         
         B     LWROTA50                                                         
         CLC   PPRDROTA,=C'BCDA'                                                
         BNE   *+14                                                             
         MVC   8(11,R2),=C'B1,C2,D3,A4'                                         
         B     LWROTA50                                                         
         CLC   PPRDROTA,=C'CDAB'                                                
         BNE   *+14                                                             
         MVC   8(11,R2),=C'C1,D2,A3,B4'                                         
         B     LWROTA50                                                         
         CLC   PPRDROTA,=C'DABC'                                                
         BNE   *+14                                                             
         MVC   8(11,R2),=C'D1,A2,B3,C4'                                         
         B     LWROTA50                                                         
*                                                                               
         DC    H'0'                NO OTHER COMBINATIONS!                       
*                                                                               
LWROTA50 OI    6(R2),X'80'                                                      
         LR    R6,R3               RESTORE R6                                   
         DROP  R6                                                               
*                                                                               
LWROTATX XIT1                                                                   
*                                                                               
L2NXTEL  CLI   0(R6),0                                                          
         BE    L2NXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     L2NXTEL                                                          
*                                                                               
L2NXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRDSECUR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SVCLSTAT,X'20'      CHK IF CLT ALLOWS OFFC & TRFF CNTL           
         BZ    PRDSEC50            NO, IT DOESN'T ALLOW THEM                    
*                                                                               
         MVC   PRDOFFT,=C'OFFICE'  DISP TITLE FIELD                             
         OI    PRDOFFTH+6,X'80'                                                 
         MVC   PRDOFFX,SPACES      ASSUME NO HEX DISP IS NEEDED                 
         MVI   PRDOFF,C' '         ASSUME NO OFFICE CODE                        
         CLI   PPRDOFFC,0          OFFICE CODE PRESENT?                         
         BE    PRDSEC20            NO, NO NEED TO GO FURTHER                    
         MVC   PRDOFF,PPRDOFFC                                                  
         LA    R5,PRDOFFXH         NEEDED FOR DISP OFFICE/TRAFFIC HEX           
         MVC   BYTE,PPRDOFFC       NEEDED FOR DISP OFFICE/TRAFFIC HEX           
         BAS   RE,DSPOTHEX                                                      
PRDSEC20 OI    PRDOFFH+6,X'80'                                                  
         OI    PRDOFFXH+6,X'80'    CONDITIONAL DISP OFFICE/TRAFFIC HEX          
*                                                                               
         MVC   PRDTRAT,=C'TRAFFIC' DISP TITLE FIELD                             
         OI    PRDTRATH+6,X'80'                                                 
         MVC   PRDTRAX,SPACES      ASSUME NO HEX DISP IS NEEDED                 
         MVI   PRDTRA,C' '         ASSUME NO TRAFFIC CODE                       
         CLI   PPRDTRAF,0          TRAFFIC CODE PRESENT?                        
         BE    PRDSEC40            NO, NO NEED TO GO FURTHER                    
         MVC   PRDTRA,PPRDTRAF                                                  
         LA    R5,PRDTRAXH         NEEDED FOR DISP OFFICE/TRAFFIC HEX           
         MVC   BYTE,PPRDTRAF       NEEDED FOR DISP OFFICE/TRAFFIC HEX           
         BAS   RE,DSPOTHEX                                                      
PRDSEC40 OI    PRDTRAH+6,X'80'                                                  
         OI    PRDTRAXH+6,X'80'    CONDITIONAL DISP OFFICE/TRAFFIC HEX          
*                                                                               
         B     PRDSECX                                                          
*                                                                               
PRDSEC50 MVC   PRDOFFT,SPACES                                                   
         OI    PRDOFFTH+6,X'80'                                                 
         MVI   PRDOFF,C' '         CLEAR OFFICE CODE FIELD IF ANY               
         OI    PRDOFFH+1,X'20'     PROTECT PRD OFFICE FIELD                     
         OI    PRDOFFH+6,X'80'                                                  
         MVC   PRDOFFX,SPACES                                                   
         OI    PRDOFFXH+6,X'80'    CLEAR HEX DISP IF ANY                        
*                                                                               
         MVC   PRDTRAT,SPACES                                                   
         OI    PRDTRATH+6,X'80'                                                 
         MVI   PRDTRA,C' '         CLEAR TRAFFIC CODE FIELD IF ANY              
         OI    PRDTRAH+1,X'20'     PROTECT PRD TRAFFIC FIELD                    
         OI    PRDTRAH+6,X'80'                                                  
         MVC   PRDTRAX,SPACES                                                   
         OI    PRDTRAXH+6,X'80'    CLEAR HEX DISP IF ANY                        
*                                                                               
PRDSECX  B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPOTHEX ST    RE,FULL             HEX PRD OFFICE/TRAFFIC DISP                  
         CLI   BYTE,X'40'          SHOW HEX VALUE?                              
         BNH   DSPOTX              NO                                           
         CLI   BYTE,X'D0'          SHOW HEX VALUE?                              
         BE    DSPOT50             YES                                          
         CLI   BYTE,X'E0'          SHOW HEX VALUE?                              
         BE    DSPOT50             YES                                          
         CLI   BYTE,C'A'           SHOW HEX VALUE?                              
         BL    DSPOT50             YES                                          
         CLI   BYTE,C'9'           SHOW HEX VALUE?                              
         BNH   DSPOTX              NO                                           
*                                                                               
DSPOT50  GOTO1 =V(HEXOUT),DMCB,BYTE,8(R5),1,RR=RELO                             
         OI    6(R5),X'80'                                                      
*                                                                               
DSPOTX   L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPRDPP NTR1  BASE=*,LABEL=*      EDITING PRD PASSIVE POINTERS                 
*                                                                               
EDTPPPX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKAOFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*&&DO                                                                           
         MVI   COMPCD,0                                                         
         MVI   ASW,0               SET TO X'01' IF SWTICHED TO ACC              
*                                                                               
         L     RF,VTWA             GET SYS NUM TO SWITCH BACK TO                
*                                                                               
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         LA    R2,PRDAOCD          POINT TO INPUT                               
         LA    R1,0                ASSUME LENGTH IS ZERO                        
         CLI   PRDAOCDH+5,0                                                     
         BE    CHKLEN                                                           
         LA    R1,1                                                             
         LA    R2,1(R2)                                                         
         CLI   PRDAOCDH+5,1                                                     
         BE    CHKLEN                                                           
         LA    R1,2                                                             
         LA    R2,2(R2)                                                         
         CLI   PRDAOCDH+5,2                                                     
         BE    CHKLEN                                                           
*                                                                               
         LA    R2,PRDAOCD          POINT TO INPUT                               
         LA    R3,5                MAXIMUM CHARACTERS CAN BE ENTERED            
         LA    R1,0                                                             
COLOOP   CLI   0(R2),C','                                                       
         BE    CHKLEN                                                           
         CLI   0(R2),C'/'                                                       
         BE    CHKLEN                                                           
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,COLOOP                                                        
*                                                                               
CHKLEN   DS    0H                                                               
         LA    R2,1(R2)            POINT AT AGY CD                              
         STC   R1,OFFLEN                                                        
         MVC   ACCOFF(2),PRDAOCD                                                
         CLI   OFFLEN,2                                                         
         BH    LENERR                                                           
         BE    CHKONE                                                           
*                                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED?                             
         BE    TWOERR                                                           
         MVI   ACCOFF+1,C' '                                                    
         CLI   OFFLEN,0                                                         
         BNE   ACCSW               SEE IF 2 CHARS MATCH                         
         XC    PPRDAOFC,PPRDAOFC                                                
         XC    PPRDACCA,PPRDACCA                                                
         MVC   PPRDAOFC,PPRDOFF    DEFAULT TO SPOT OFFICE IF <2 CHARS           
         MVI   PPRDAOFC+1,C' '                                                  
         B     COXX                                                             
*                                                                               
CHKONE   CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED?                             
         BNE   ONEERR                                                           
         B     ACCSW                                                            
*                                                                               
LENERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(32),=C'** ERROR - INVALID OFFICE LENGTH'                  
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
TWOERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - 2 CHAR OFFICE CODE REQUIRED'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
ONEERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - 1 CHAR OFFICE CODE REQUIRED'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
AGYERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID AGENCY CODE'                    
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
DIFACC   DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R3,SVACCAGY                                                      
         LA    R1,4                                                             
CKAGYLP  CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    GETSE                                                            
         CLI   0(R3),C' '                                                       
         BNH   AGYERR                                                           
         LA    R3,2(R3)                                                         
         BCT   R1,CKAGYLP                                                       
         B     AGYERR                                                           
*                                                                               
GETSE    MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY         ACC AGY CODE                                 
         LA    R6,CTKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,PRDTAB             
         LA    R6,PRDTAB                                                        
*                                                                               
         CLI   8(R1),0             ERRORS?                                      
         BE    CO3                                                              
*                                                                               
CO3ERR   DS    0H                  ERROR IF NOT FOUND                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID ACC AGY CODE'                   
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO3      MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BAS   RE,GETEL                                                         
         BE    CO3A                                                             
         B     CO3ERR              ERROR IF NOT FOUND                           
CO3NX    BAS   RE,NEXTEL                                                        
*                                                                               
CO3A     DS    0H                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   CO3ERR              ERROR IF NOT FOUND                           
*                                                                               
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   CO3NX                                                            
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,VTWA             SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
*                                                                               
         GOTO1 (RF),DMCB                                                        
         B     ACCSW2                                                           
*                                                                               
ACCSW    DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   ACCSW1                                                           
         MVC   POWCODE,0(R2)                                                    
         B     DIFACC              SWITCH TO DIFFERENT ACC SYSTEM               
*                                                                               
ACCSW1   DS    0H                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED                              
         BNE   CO5                 IF NOT THEN SKIP SWITCH                      
*                                                                               
         L     RF,VTWA             SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
*                                                                               
ACCSW2   CLI   4(R1),2             SYSTEM NOT OPEN                              
         BNE   CO1                                                              
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(33),=C'** ERROR - ACC SYSTEM IS NOT OPEN'                 
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO1      CLI   4(R1),1             ANY OTHER ERRORS?                            
         BNE   CO1A                SHOULDN'T BE A BNH                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - CANNOT SWITCH TO ACC SYSTEM'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO1A     DS    0H                                                               
         MVI   ASW,X'01'           SET ASW TO 1 WHEN SWITCHING TO ACC           
*                                  SUCCESSFULLY                                 
         CLI   COMPCD,0            SEE IF I ALREADY HAVE                        
         BNE   CO1AA               (FROM CTFILE)                                
*                                                                               
         CLI   0(R1),0             NO RETURNED CODE                             
         BE    CO1ERR                                                           
         MVC   COMPCD,0(R1)        SAVE RETURNED AGENCY BINARY CODE             
*                                  READ COMPANY REC                             
CO1AA    MVC   MYACCKEY,SPACES                                                  
         MVC   MYACCKEY(1),COMPCD  RETURNED AGENCY BINARY CODE                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,PRDTAB             
         CLI   8(R1),0                                                          
         BE    CO1AB                                                            
*                                                                               
CO1ERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(36),=C'** ERROR - ACC COMPANY REC NOT FOUND'              
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         BAS   RE,RETURN           RETURN TO MY SYSTEM                          
         B     NO                                                               
*                                                                               
CO1AB    LA    R6,PRDTAB                                                        
*                                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED?                             
         BNE   CO5                                                              
         LA    R6,MYACCKEY         NEW OFFICE, LOOK FOR OFFICE REC              
         USING OFFRECD,R6                                                       
         MVC   MYACCKEY,SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ    X'01'                                        
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,PRDTAB             
         CLI   8(R1),0                                                          
         BE    CO5                                                              
*                                                                               
         DS    0H                  ERROR IF NOT FOUND                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,PRDAOCDH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID ACC OFF CODE'                   
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         BAS   RE,RETURN           RETURN TO MY SYSTEM                          
         B     NO                                                               
*                                                                               
CO5      DS    0H                  OFFICE CODE IS GOOD                          
         MVC   PPRDAOFC,ACCOFF     SAVE OFFICE CODE                             
         MVC   PPRDACCA,POWCODE    SAVE AGY CODE                                
         CLI   ASW,0               SEE IF I SWITCHED TO AN ACC SYSTEM           
         BE    COXX                                                             
         BAS   RE,RETURN           MUST RETURN TO MY SYSTEM                     
*                                                                               
COXX     XIT1                                                                   
*                                                                               
YES      SR    RE,RE                                                            
NO       LTR   RE,RE                                                            
*                                                                               
         DS    F                                                                
RETURN   ST    RE,RETURN-4                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,VTWA             SWITCH BACK                                  
*                                                                               
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'33'     FOR PRINT                                    
         L     RE,RETURN-4                                                      
         BR    RE                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
SVACCOFC DS    CL1       SET TO 'Y' IF SVAGPF12 IS A OR B                       
COMPCD   DS    CL1                                                              
ASW      DS    XL1       SET OT X'01' IF I SWITCHED TO ACC                      
MYACCKEY DS    CL42                                                             
MYCTKEY  DS    CL48      TO VALIDATE T/A RFP GROUP IN GENDIR                    
CTKEY    DS    CL28                                                             
SENUM    DS    XL1                                                              
SYSSW    DS    XL1                                                              
DATADISP DS    H                                                                
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    XL1                                                              
*                                                                               
*&&                                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM BASE=* SUBROUNTINES                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PUGENOLD                                                       
*                                                                               
         DS    2000C                                                            
*                                                                               
         ORG   IOAREA                                                           
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PCLTREC                                                        
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PPRDREC                                                        
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PESTREC                                                        
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PDIVREC                                                        
*                                                                               
         ORG   IOAREA                                                           
         DS    CL500                                                            
ZZZIO    DS    0CL300                                                           
         DS    CL55                                                             
STDATE   DS    CL6                 START/END DATES OF EST IN ZZZIO              
ENDDATE  DS    CL6                                                              
*                                                                               
         ORG   KEY                                                              
KAGY     DS    CL2                                                              
KMED     DS    CL1                                                              
KRCD     DS    CL1                                                              
KCLT     DS    CL3                                                              
KPRD     DS    CL3                                                              
KEST     DS    CL2                                                              
         DS    CL20                                                             
         ORG                                                                    
*                                  NOW AT IOAREA+2000                           
DSPSW    DS    X                                                                
DONESW   DS    X                                                                
PFKEY    DS    XL1                                                              
         DS    XL1                 SPARE                                        
*                                                                               
VGLOBBER DS    A                                                                
ATIOB    DS    A                                                                
         DS    CL40                SPARE WORK AREA BYTES                        
*                                                                               
PUBIO    DS    0CL4000             ** NOTE PUBIO IS NOT USED IN LFM **          
*                                  THIS TAG IS ONLY HERE TO PREVENT             
*                                  ASSEMBLY ERRORS CAUSED BY PGENEROL           
PRDTAB   DS    CL2000                                                           
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE PPLFMFFD                                                       
*                                                                               
         ORG   T404FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
CLTADDR  DS    F                                                                
PRDADDR  DS    F                                                                
ESTADDR  DS    F                                                                
CPROFLE  DS    CL20                                                             
LDONE    DS    CL1                                                              
LACT     DS    CL1                                                              
LREC     DS    CL1                                                              
LMED     DS    CL1                                                              
LCLT     DS    CL3                                                              
LEST     DS    CL2                                                              
SCRNUM   DS    CL1                                                              
SVAGPF12 DS    CL1                                                              
FINANSW  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF2D          MAINTAINANCE SCREEN                          
         ORG   HDRLAST+2000                                                     
WNATION  DS    CL1                                                              
*                                                                               
SVP1USER DS    CL20                PRD USER DESCRIPTION FIELD 1                 
SVP1TYPE DS    CL1                          TYPE                                
SVP1LEN  DS    XL1                          LENGTH                              
SVP1FLG1 DS    XL1                          FLAG                                
SVP1FLG2 DS    XL1                          FLAG                                
SVP2USER DS    CL20                PRD USER DESCRIPTION FIELD 2                 
SVP2TYPE DS    CL1                          TYPE                                
SVP2LEN  DS    XL1                          LENGTH                              
SVP2FLG1 DS    XL1                          FLAG                                
SVP2FLG2 DS    XL1                          FLAG                                
SVE1USER DS    CL20                EST USER DESCRIPTION FIELD 1                 
SVE1TYPE DS    CL1                          TYPE                                
SVE1LEN  DS    XL1                          LENGTH                              
SVE1FLG1 DS    XL1                          FLAG                                
SVE1FLG2 DS    XL1                          FLAG                                
SVE2USER DS    CL20                EST USER DESCRIPTION FIELD 2                 
SVE2TYPE DS    CL1                          TYPE                                
SVE2LEN  DS    XL1                          LENGTH                              
SVE2FLG1 DS    XL1                          FLAG                                
SVE2FLG2 DS    XL1                          FLAG                                
SVULNQ   EQU   *-SVP1USER                                                       
*                                                                               
F0PROF   DS    CL16                F0 PROFILE READ IN 00                        
*                                  WHEN CLIENT IS VALIDATED                     
SVACCAGY DS    CL24                ROOM FOR 12 ACC AGENCYS                      
SVCTAGY  DS    CL2                 CTFILE ID                                    
*                                                                               
SVXFRSY  DS    CL3                 TRANSFERRED FROM SYSTEM                      
SVXFRPR  DS    CL3                 TRANSFERRED FROM PROGRAM                     
*                                                                               
SVCLSTAT DS    XL1                 PCLTSTAT FROM PCLTREC SET IN 00              
*                                  WHEN CLIENT IS VALIDATED                     
SVAGYSW  DS    XL1                 AGENCY "SWITCH" SET IN 00 AT CKVALC          
*                                  X'01' = WESTERN AGENCY (OR SJR)              
SVSTEREO DS    CL1                 'Y' IF STEREO                                
*                                                                               
SVAGPINI DS    XL2                 PAGYPINI (BIN RFP ID #) FROM AGY HDR         
*                                  SET IN 00 AT VALIDATE MEDIA (CKMED)          
*                                                                               
SVPOFFCD DS    X'00'               SAVE PRD OFFICE CODE                         
SVPTRACD DS    X'00'               SAVE PRD TRAFFIC CODE                        
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UDEFD    DSECT                                                                  
UDEF     DS    CL20                                                             
UTYPE    DS    CL1                                                              
ULEN     DS    XL1                                                              
UFLG1    DS    XL1                                                              
UFLG2    DS    XL1                                                              
         EJECT                                                                  
PCONRECD DSECT                                                                  
       ++INCLUDE PCONREC                                                        
         EJECT                                                                  
POTHRECD DSECT                                                                  
       ++INCLUDE POTHAGY                                                        
         EJECT                                                                  
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
*                                                                               
PPRDADJD DSECT                                                                  
       ++INCLUDE PADJPRDPP                                                      
PPRDOFFD DSECT                                                                  
       ++INCLUDE POFFPRDPP         PRODUCT OFFICE PASSIVE POINTER               
PPRDTRAD DSECT                                                                  
       ++INCLUDE PTRAPRDPP         PRODUCT TRAFFIC PASSIVE POINTER              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE         CONTROL FILE DSECTS                          
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPLFM02   09/06/12'                                      
         END                                                                    
