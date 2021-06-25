*          DATA SET PPLFM02S   AT LEVEL 005 AS OF 05/01/02                      
*PHASE T40402A                                                                  
*                                                                               
         TITLE 'T40402   CHANGE LOG    FILE MAINT.  PRD. SCREEN'                
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
*                                                                               
         TITLE 'T40402   PRINT LOGICAL FILE MAINT.  PRD. SCREEN'                
T40402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40402,R9                                                      
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         LA    R9,T40402+4095                                                   
         LA    R9,1(R9)                                                         
         USING T40402+4096,R9                                                   
         USING   T404FFD,RA                                                     
         EJECT                                                                  
         LA    R4,IOAREA                                                        
         LA    R5,1000                                                          
         BAS   RE,CLEARWRK                                                      
         MVC   PPRDKEY,KEY                                                      
         MVC   PPRDELEM(2),=X'06C8' WAS X'06AA' NEW ELM LENGTH 200              
         MVC   PPRDDIV,=4C'0'                                                   
         MVC   PPRDLEN,=X'00E9' WAS X'CB'/NEW RECORD LEN 233                    
         CLI   SCRNUM,X'F2'                                                     
         BE    PRD2                                                             
         MVI   DSPSW,1                                                          
*                                                                               
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F2'                                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
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
OHCANADA DS    0H                                                               
*                                                                               
         MVI   SCRNUM,X'F2'                                                     
         CLI   CPROFLE,C'0'                                                     
         BNE   PRD1                                                             
         XC    PRDDIVA,PRDDIVA         CLEAR DIV ANNO                           
         XC    PRDDIV,PRDDIV                                                    
         OI    PRDDIVH+1,X'20'                                                  
         MVI   PRDDIVH+5,0                                                      
PRD1     DS    0H                                                               
         BAS   RE,SETUSER                                                       
         CLI   BACT,1                                                           
         BE    NOTDONE                                                          
         B     *+8                                                              
PRD2     DS    0H                                                               
         BAS   RE,SETUSER                                                       
         CLI   BACT,1                                                           
         BE    PRDSCRN                                                          
         MVC   KEY+27(4),PRDADDR                                                
         BAS   RE,GETREC                                                        
PRDSCRN  XC    PRDOANN,PRDOANN                                                  
         FOUT  PRDOANNH                                                         
         FOUT  HDRPRDNH                                                         
         CLC   HDRPRDN+20(3),OANMSG   IF = MUST CLEAR MSG                       
         BNE   *+10                                                             
         XC    HDRPRDN+20(L'OANMSG),HDRPRDN+20                                  
         CLI   DSPSW,0                                                          
         BNE   FORMATP                                                          
*                        PRODUCT SCREEN IN TWA SO EDIT IT UNLESS                
*                        ACTION=DISPLAY                                         
         CLI   BACT,X'03'                                                       
         BE    FORMATP                                                          
         LA    R2,PRDPRDNH                                                      
         BAS   RE,ANY                                                           
         CLC   8(6,R2),=C'DELETE'                                               
         BE    DELETE                                                           
         XC    PPRDNAME,PPRDNAME                                                
         MVC   PPRDNAME,PRDPRDN                                                 
*                                                                               
JWTTLA   LA    R2,PRDOANH                                                       
*  CANNOT CHANGE OAN PRODUCT TO A NON-OAN PRODUCT                               
         CLI   BACT,1       IS THIS AN ADD                                      
         BNE   ISACHANG                                                         
         CLI   5(R2),0       ANY INPUT                                          
         BE    MOVEAORN                                                         
         B     YMOVEAOR      ADD OAN IF VALID // SEE IF CODE OK                 
ISACHANG DS    0H                                                               
         CLI   5(R2),0       ANY INPUT-- IF NONE CHECK TO SEE IF                
         BE    OANINPRD   OTHER AGENCY NAME IN PRODUCT                          
         CLC   PPRDOAN,=X'0000'  IF NO OAN CODE PRESENT THEN CANNOT             
         BE    NOCANDO           CHANGE TO AN EXISTING OAN CODE                 
         B     YMOVEAOR      VERIFY EXISTANCE                                   
*        *                                                                      
OANINPRD CLC   PPRDOAN,=X'0000'  IF BIN ZERO    NO OAN CODE                     
         BE    MOVEAORN     NOTHING ENTERED                                     
         FOUT  HDRMSGH,=C'****CANNOT*REMOVE*OAN*CODE****',30                    
         B     ZIPOUT                                                           
NOCANDO  FOUT  HDRMSGH,=C'****CANNOT*CHANGE*OAN*CODE****',30                    
         B     ZIPOUT                                                           
*                                                                               
YMOVEAOR DS    0H    SEE IF OTHER AGENCY NAME RECORD EXISTS                     
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),PPRDKEY  AGENCY + MEDIA                                   
         MVI   KEY+3,X'16'     ID                                               
         MVC   KEY+4(2),PRDOAN                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BE    YMOVEOK                                                          
         XC    PRDOANN,PRDOANN                                                  
         FOUT  PRDOANNH                                                         
         FOUT  HDRPRDNH                                                         
         CLC   HDRPRDN+20(3),OANMSG   IF =  CLEAR MSG                           
         BNE   *+10                                                             
         XC    HDRPRDN+20(L'OANMSG),HDRPRDN+20                                  
         FOUT  HDRMSGH,=C'***OAN CODE NONEXISTANT  *****',30                    
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
*****                                                                           
         MVC   PPRDOAN,PRDOAN                                                   
         MVC   OANMSG+21(2),PPRDOAN   MOVE                                      
         MVC   HDRPRDN+20(L'OANMSG),OANMSG                                      
         NI    HDRPRDNH+7,X'80'                                                 
         OI    HDRPRDNH+7,X'50'       NEW LENGTH OF OUTPUT                      
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GET2ZIO  READ PRD HDR INTO ZZZIO TO PRIME D/M                    
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
         BE    CKACCNT                                                          
         MVC   PPRDATTN,8(R2)                                                   
*                                                                               
CKACCNT  LA    R2,PRDACCTH                                                      
         LA    R3,FLDINV                                                        
         XC    PPRDACCT,PPRDACCT                                                
         CLI   5(R2),0                                                          
         BE    CKDIV                                                            
         TM    4(R2),X'08'         TEST FOR NUMERICS                            
         BNO   CKPACCT    NO                                                    
         BAS   RE,PACK                                                          
         ST    R0,FULL                                                          
         MVC   PPRDACCT+1(3),FULL+1                                             
         MVI   PPRDACCT,X'FF'                                                   
         B     CKDIV                                                            
CKPACCT  CLI   5(R2),4            IF NOT NUMERIC THEN ONLY 4 CHARS              
         BH    ERROR                                                            
         MVC   PPRDACCT,PRDACCT                                                 
         OC    PPRDACCT,=4C' '                                                  
         SPACE 2                                                                
CKDIV    DS    0H                                                               
         MVI   PPRDGST,0                                                        
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   CKDIVAA                                                          
         LA    R2,PRDGSTH                                                       
         LA    R3,2                                                             
******************                                                              
* SOME SORT OF CUT OFF DATE IS NEEDED TO PREVENT CHANGING                       
* AFTER A CERTAIN DATE                                                          
******************                                                              
         CLI   5(R2),0              ANY INPUT                                   
         BE    CKDIVAA               NO- O.K                                    
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
         CLI   CPROFLE,C'1'        SEE IF DIVISIONS REQUIRED                    
         BE    CKDIV1                                                           
         CLI   CPROFLE,C'2'                                                     
         BNE   PRDSCRN1                                                         
CKDIV1   BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         LA    R3,DIVERR1                                                       
         XC    PPRDDIV,PPRDDIV                                                  
         OI    DUB+7,X'0F'                                                      
         UNPK  PPRDDIV(3),DUB+6(2)                                              
         BAS   R8,DIVREAD                                                       
         B     PRDSCRN2                                                         
*                                                                               
PRDSCRN1 XC    PRDDIVN,PRDDIVN                                                  
         FOUT  PRDDIVNH                                                         
         LA    R3,DIVERR2                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         XC    PPRDDIV,PPRDDIV                                                  
*                                                                               
PRDSCRN2 LA    R6,PPRDELEM                                                      
*                                                                               
PRDSCRN3 CLI   0(R6),0                                                          
         BE    PRDSCRN5                                                         
         CLI   0(R6),X'08'                                                      
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
         MVC   PRDDSC2,WORK        CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDDSC2H+6,X'80'                                                 
         DROP  R4                                                               
*                                                                               
         OC    ELEM+2(L'PUSER1+L'PUSER2),ELEM+2                                 
         BZ    EDT12C                                                           
         MVC   ELEM(2),=X'0832'    ELEMENT CODE/LENGTH                          
         GOTO1 VRECUP,DMCB,(1,PPRDREC),ELEM,0(R6)                               
*                                                                               
EDT12C   LA    R2,PRDPSTH                                                       
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   EDT16                                                            
         CLI   5(R2),0                                                          
         BE    EDT16                                                            
         BAS   RE,VALPST                                                        
         BNE   INVERR                                                           
*                                                                               
EDT16    DS    0H                                                               
         LA    R2,PRDADJSH                                                      
         MVC   SAVADJ,PPRDEXCL    SAVE 'OLD' ADJACENCY CODES                    
*                                                                               
         XC    PPRDEXCL,PPRDEXCL                                                
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         CLI   5(R2),3           MAX IS 3                                       
         BH    INVERR                                                           
*                                                                               
         MVC   PPRDEXCL,PRDADJS                                                 
*                                  CHECK FOR DUPLICATES                         
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
*                                                                               
*                                                                               
EDT20    DS    0H                                                               
         B     EDT30                                                            
*                                                                               
**       MVI   PPRDEXC,0                                                        
**       LA    R2,PRDEXCLH         EXCLUSION CLASS FIELD                        
**       CLI   5(R2),0                                                          
**       BE    EDT30                                                            
**                                                                              
**       CLI   5(R2),9             MAX IS 9                                     
**       BH    INVERR                                                           
**                                                                              
**       ZIC   RE,5(R2)                                                         
**       LA    R1,PRDEXCL          POINT TO INPUT FIELD                         
**                                                                              
**NDEXCL LA    RF,EXCLTAB                                                       
**EXCL   CLC   0(1,R1),0(RF)                                                    
**       BE    MOVEEXCL                                                         
**       CLC   2(2,RF),=X'0000'                                                 
**       BE    INVERR              NOT IN TABLE                                 
**       LA    RF,2(RF)                                                         
**       B     CKEXCL                                                           
**                                                                              
**VEEXCL OC    PPRDEXC,1(RF)       "OR" BIT INTO RECORD                         
**       AHI   RE,-2                                                            
**       LTR   RE,RE                                                            
**       BNP   EDT30                                                            
**       CLI   1(R1),C','                                                       
**       BNE   INVERR              INVALID FORMAT IN INPUT                      
**       LA    R1,2(R1)                                                         
**       B     FINDEXCL                                                         
**                                                                              
**               B   W   L   T   C                                              
**CLTAB  DC    X'C280E640D320E310C3080000'                                      
*                                                                               
*                                                                               
*                                                                               
EDT30    DS    0H                  FOR FUTURE USE                               
*                                                                               
         CLI   BACT,X'01'                                                       
         BNE   PRDCHG                                                           
         BAS   RE,ADDREC                                                        
         MVC   PRDADDR,KEY      SAVE KEY FOR ESTIMATE ADD                       
         B     ADDPTRS                                                          
*                                                                               
PRDCHG   BAS   RE,PUTREC                                                        
*                              ADD PASSIVE POINTERS HERE                        
ADDPTRS  DS    0H                                                               
         MVC   SAVDMIN,DMINBTS   SAVE 'REAL' DMINBTS                            
         MVC   SAVDMOUT,DMOUTBTS  AND 'REAL DMOUTBTS                            
         MVI   DMINBTS,X'08'     SET TO PASS DELETES                            
         MVI   DMOUTBTS,0        SUPPRESS DATAMGR ERRORS                        
*                                                                               
         CLC   SAVADJ,PPRDEXCL   SEE IF CODES CHANGED                           
         BE    ADDPX                                                            
         OC    SAVADJ,SAVADJ     SEE IF I HAD OLD                               
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
         DC    H'0'           SHOULD FIND                                       
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
*                                                                               
ADDP20   LA    R6,1(R6)                                                         
         BCT   R4,ADDP5                                                         
*                                                                               
ADDP50   OC    PPRDEXCL,PPRDEXCL    SEE IF I HAVE ANY NEW                       
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
*                                   IF NOT FOUND - THEN ADD POINTER             
         XC    KEY,KEY                                                          
         MVC   KEY(25),KEYSAVE                                                  
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 ADD                                                              
         B     ADDP70                                                           
*                                                                               
ADDP58   MVI   KEY+25,0          UNDELETE POINTER                               
         MVC   KEY+27(4),PRDADDR                                                
         GOTO1 WRITE                                                            
*                                                                               
ADDP70   LA    R6,1(R6)                                                         
         BCT   R4,ADDP52         NEXT ADJACENCY CODE                            
*                                                                               
ADDPX    DS    0H                                                               
         MVC   DMINBTS,SAVDMIN    RESTORE DMINBTS AND DMOUTBTS                  
         MVC   DMOUTBTS,SAVDMOUT                                                
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R6,PPRDELEM                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   VP10                                                             
*                                        CHANGE - DELETE OLD ELEM               
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
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
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
*        DISPLAY PST CODES                                                      
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
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   PRDPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET USER DESCRIPTION FIELDS ON THE SCREEN                              
*                                                                               
SETUSER  NTR                                                                    
         MVC   MYAREA(L'SVP1USER),SVP1USER    DISPLAY 1ST DESC LINE             
         LA    R1,PRDDEF1H                                                      
         LA    R6,PRDDSC1H                                                      
         BAS   RE,FMTUSR                                                        
*                                                                               
         MVC   MYAREA(L'SVP2USER),SVP2USER    DISPLAY 2ND DESC LINE             
         LA    R1,PRDDEF2H                                                      
         LA    R6,PRDDSC2H                                                      
         BAS   RE,FMTUSR                                                        
*                                                                               
SUX      XIT                                                                    
         EJECT                                                                  
*                                                                               
*        PUT OUT USER DESCRIPTION FIELDS                                        
*             R1   = A(DESC FIELD).                                             
*             R6   = A(INPUT FIELD).                                            
*             DESC = PRODUCT DESCRIPTION.                                       
*                                                                               
FMTUSR   DS    0H                                                               
         OI    1(R6),X'20'              PROTECT INPUT FIELD                     
         MVC   8(L'SVP1USER,R1),MYAREA  CLEAR ANY PREVIOUS DESC FIELDS          
         CLC   SPACES(L'SVP1USER),MYAREA NEED TO SHOW DESCRIPTION               
         BL    FMTUSR10                                                         
         LR    R0,R1                    SAVE C(R1) AROUND.                      
         ZIC   R1,0(R6)                 R1=L(HEADER)+L(INPUT FIELD)             
         SH    R1,=H'8'                 R1=L(INPUT FIELD)                       
         TM    1(R6),X'02'              CHECK FOR EXTENDED HEADER               
         BZ    *+8                                                              
         SH    R1,=H'8'                 SUBTRACT L(X-HEADER)                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)            CLEAR ANY GARBAGE                       
         LR    R1,R0                                                            
         B     FMTX                                                             
*                                                                               
FMTUSR10 NI    1(R6),X'FF'-X'20'        UNPROTECT INPUT FIELD                   
*                                                                               
FMTX     OI    6(R1),X'80'              TRANSMIT FIELD                          
         OI    6(R6),X'80'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*         INPUT:                                                                
*             R2 = A(INPUT FIELD)                                               
*             R3 = USER BLOCK FROM CLIENT RECORD                                
*         OUTPUT:                                                               
*             WORK = DATA OR NULLS                                              
*                                                                               
EDTUSR   DS    0H                                                               
         USING UDEFD,R3                                                         
         ST    RE,SVRE                                                          
         ST    R4,SVR4                                                          
         XC    WORK,WORK                                                        
         OC    0(L'SVP1USER,R3),0(R3)  ANY INFO                                 
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
FORMATP  DS    0H                                                               
*                                                                               
PUTFLD   FOUT  PRDPRDNH,PPRDNAME,20                                             
*                                                                               
         XC    PRDADJS,PRDADJS                                                  
****     XC    PRDEXCL,PRDEXCL                                                  
*                                                                               
         FOUT  PRDADJSH,PPRDEXCL,3                                              
         FOUT  PRDOANH,PPRDOAN,2                                                
         FOUT  PRDBRNH,PPRDBILL,20                                              
         FOUT  PRDBRN2H,PPRDBIL2,20                                             
         FOUT  PRDAL1H,PPRDLIN1,30                                              
         FOUT  PRDAL2H,PPRDLIN2,30                                              
         FOUT  PRDATTNH,PPRDATTN,24                                             
*                                                                               
***      BAS   RE,PUTEXCL          DISPLAY EXCLUSION CLASS FIELDS               
*NO-OPED*                                                                       
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
         FOUT  HDRMSGH,=C'***OAN CODE NONEXISTANT  *****',30                    
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
         MVC   KEY,SAVEKEY   PRODUCT HEADER                                     
         MVC   OANMSG+21(2),PPRDOAN   MOVE                                      
         MVC   HDRPRDN+20(L'OANMSG),OANMSG                                      
         NI    HDRPRDNH+7,X'80'                                                 
         OI    HDRPRDNH+7,X'50'       NEW LENGTH OF OUTPUT                      
         GOTO1 HIGH          RESET DIRECTORY                                    
         GOTO1 GET2ZIO  READ PRD HDR INTO ZZZIO TO PRIME D/M                    
*****                                                                           
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
*  READ OTHERAGENCY NAME INTO ZZZIO                                             
*                                                                               
         PRINT GEN                                                              
GET2ZIO  NTR                                                                    
         PRINT NOGEN                                                            
         MVC   COMMAND,=C'GETREC'                                               
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),ZZZIO,(TERMNAL,DMWORK)                                      
         B     DMCHECK                                                          
*                                                                               
*        FIND UDEF ELEMENT & DISPLAY INFO                                       
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
*                                                                               
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
*                                                                               
*                  B   W   L   T   C                                            
EXCLTAB1 DC    X'7FC2BFE6DFD3EFE3F7C30000'                                      
*                                                                               
*                                                                               
PUTEXCL9 OI    PRDEXCLH+6,X'80'    TRANSMITT TO SCREEN                          
*                                                                               
PUTEXCLX B     XIT                                                              
*                                                                               
*                                                                               
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
         CLI   KEY+3,X'08'        SEE IF BILL                                   
         BNE   DEL16                                                            
         OC    KEY+10(2),KEY+10     WITH EST =0                                 
         BZ    DEL16                                                            
         DC    H'0'           SHOULD NEVER HAPPEN                               
*                             SINCE THIS BILL SHOULD HAVE CAUSED                
*                             DELERR IN DEL6 LOGIC                              
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
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+25,X'80'        DELETE POINTER                               
         GOTO1 WRITE                                                            
         B     DEL22                                                            
*                                                                               
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
*        NOW I CAN DELETE THE PRODUCT ITSELF                                    
*                                                                               
DEL50    MVC   KEY+27(4),PRDADDR                                                
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
         NI    HDRPRDH+4,X'DF'     UNVALIDATE PRD                               
         B     EXXMOD                                                           
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
******   MVC   IOAREA+500(250),IOAREA                                           
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
*******  MVC   IOAREA(250),IOAREA+500                                           
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
DONE     MVI   DONESW,1                                                         
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
PRDTAB1  DC    X'0708201400'         ESTS,INVS,BUYS,AOR                         
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
VIRERR   DC    H'0'                                                             
SAVADJ   DS    CL3       OLD ADJACENCY CODES                                    
SAVDMIN  DS    CL1                                                              
SAVDMOUT DS    CL1                                                              
*                                                                               
DIVERR1  EQU   91                                                               
DIVERR2  EQU   92                                                               
DIVERR3  EQU   93                                                               
DELERR   EQU   198                                                              
DUPEDATA EQU   170                                                              
ELEM     DS    CL255                                                            
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF2SD                                                      
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
PPRDADJD DSECT                                                                  
       ++INCLUDE PADJPRDPP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPLFM02S  05/01/02'                                      
         END                                                                    
