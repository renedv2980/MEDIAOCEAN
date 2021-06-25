*          DATA SET DECNSOR4   AT LEVEL 080 AS OF 11/22/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNSR4A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DDINFO                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'SORT CABLE NAD TAPE BEFORE RUNNING CONVERSION'                  
DECABSRT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DECABSRT,=V(REGSAVE),R8                                        
         USING MITCND,R2             R2 = MIT DSECT                             
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         MVC   TITLE,=CL60'CABLE TAPE PRE-CONVERSION SORT'                      
*                                                                               
INIT     OPEN  (FILIN,(INPUT))                                                  
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
         XC    INCNT,INCNT                                                      
         XC    SVKEY,SVKEY         SAVED PRG/TRK/TEL/ORIGIN                     
         LA    RE,IOAREA                                                        
         LHI   R1,RECLENQ                                                       
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
         LAY   RE,CHGBUFF                                                       
         L     RF,CHGBUFLN                                                      
         XCEF                                                                   
         BAS   RE,INPTOP           RD TAPE,BLD SORTKEY,POST TO SORTER           
         BAS   RE,MERGE            RD AND MERGE/DELETE RECS FROM SORT           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*INPTOP - PICK UP RECDS FROM TAPE GET RID OF UNWANTED RECDS,                    
*  POST RECORDS TO SORTER PREFIXED WITH SORTKEY.                                
*  SORTEKY=MITKEY EXCEPT FOR 04 PRG RECDS WHICH HAVE A SPECIAL SORTKEY          
***********************************************************************         
INPTOP   NTR1                                                                   
INPLP    LA    R5,SORTKEY                                                       
         USING SRTKD,R5                                                         
         GET   FILIN,IOAREA                                                     
*        MVC   P,IOAREA                                                         
*        GOTO1 =V(PRINTER)                                                      
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA                                                        
         USING MITCND,R2            R2 = MIT DSECT                              
         MVC   SORTKEY,MITKEY      DEFAULT SORTKEY=ORIG KEY ON RECD             
*                                                                               
         CLC   MITSEQ,=C'00'       PROGRAM DESCRIPTOR RECD                      
         BNE   INP01                                                            
         MVC   ORIGSDT,MITSTART-1                                               
         B     INPPOST             PUT RECD TO SORTER                           
*                                                                               
INP01    DS    0H                                                               
*                                                                               
         CLC   MITSEQ,=C'01'       UNIV RECDS                                   
         BE    INPPOST               JUST RELEASE AS IS                         
*                                                                               
INP02    CLC   MITSEQ,=C'02'       INTABS                                       
         BE    INPPURG               NOT SUPPORTED, DROP FROM SORT              
*                                                                               
INP03    CLC   MITSEQ,=C'03'       TVU                                          
         BNE   INP04                                                            
         MVC   MITPRG+4(6),MITCOVG     NEED CVG HI IN KEY FOR SORT              
         MVC   MITPRG-MITCND+4(6,R5),MITCOVG                                    
         CLC   MITHLFID,SPACES     IS HH NUMBER PROVIDED?                       
         BNE   *+8                 YES                                          
         BAS   RE,GETHH            NO. COMPUTE IT                               
         B     INPPOST                                                          
*                                                                               
INP04    DS    0H                  PRG RECD HANDLING                            
         CLC   MITSEQ,=C'04'                                                    
         BNE   INP05                                                            
         CLI   MITREC,C'E'         EXCLUDE THIS RECORD                          
         BE    INPPURG                                                          
*        CLC   MITCOVG,=C'007443' SV BYPASS                                     
*        BE    INPPURG                                                          
*        CLC   MITHLFID,ZEROS      PURGE PRG 1/2HR RECDS + PRG PUTS             
*        BNE   INPPURG                                                          
*        CLI   MITPUT,C'1'         NOT PROVIDED ON CABLE NAD                    
*        BE    INPPURG                                                          
         CLC   MITAVG,=C'00'       FOR INDIVIDUAL DAYS,                         
         BNE   *+14                GET RID OF THE TRACK INFORMATION             
         CLC   MITTRACK,SPACES                                                  
         BNE   INPPURG                                                          
         MVC   SORTKEY,SPACES      FOR '04' REC, SORT BY PRG/TRK/TEL            
         MVC   SRTSEQ,MITSEQ                                                    
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         STCM  RE,15,SRTCTR        KEEP RECS IN SEQ THEY CAME TO US IN          
         MVC   SRTBK,ORIGSDT       SET BOOK IN SORTKEY                          
         CLI   MITCORDT,C' '                                                    
         BNE   CBUFFOK                                                          
         LAY   RE,CHGBUFF                                                       
         CLI   0(RE),0                                                          
         BE    CBUFFOK                                                          
         L     RF,CHGBUFLN                                                      
         XCEF                                                                   
CBUFFOK  CLI   MITORIG,C'0'                                                     
         BE    *+10                                                             
         MVC   SRTBK,MITCORDT      SET FROM REPROC RECD                         
         MVC   SRTSTA,MITCOVG                                                   
         MVC   SRTPRG,MITPRG                                                    
         MVC   SRTTRK,MITTRACK                                                  
         MVC   SRTTELC,MITTELC                                                  
         MVC   SRTORIG,MITORIG                                                  
         CLI   MITORIG,C'1'        SRT CHG(MODIFY) RECDS AFTER DELETES          
         BNE   INPCDEL                                                          
         MVI   SRTORIG,C'M'        MODIFY                                       
         MVC   PRCHGKEY(L'PRCHGKEY),SRTBK      SAVE CHANGE KEY                  
         LAY   RF,CHGBUFF                                                       
CBFFILL  CLC   PRCHGKEY,0(RF)                                                   
         BE    INPCDEL                                                          
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,L'PRCHGKEY(RF)                                                
         B     CBFFILL                                                          
         MVC   0(L'PRCHGKEY,RF),PRCHGKEY                                        
INPCDEL  CLI   MITORIG,C'2'                                                     
         BNE   INPPOST                                                          
         MVI   SRTORIG,C'D'        DELETE                                       
*        CLC   SRTBK(L'PRCHGKEY),PRCHGKEY                                       
*        BE    INPPURG                                                          
         LAY   RF,CHGBUFF                                                       
CBFMTCH  CLC   SRTBK(L'PRCHGKEY),0(RF)                                          
         BE    INPPURG                                                          
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,L'PRCHGKEY(RF)                                                
         B     CBFMTCH                                                          
         B     INPPOST             POST RECD TO SORT                            
*                                                                               
INP05    CLC   MITSEQ,=C'05'       TVU                                          
         BNE   INP99                                                            
         CLC   MITHLFID,SPACES     IS HH NUMBER PROVIDED?                       
         BNE   *+8                 YES                                          
         BAS   RE,GETHH            NO. COMPUTE IT                               
* NEED TO SORT THE '05' RECORDS BY HH, AND THEN BY MARKET BREAK AND             
* DEMOGRAPHIC GROUP. SHIFT FIELDS AROUND                                        
SRTK     USING MITCND,SORTKEY                                                   
         MVC   SRTK.MITSEC(L'MITREC),SRTK.MITREC                                
         MVC   SRTK.MITQTRID(L'MITMKTBR),SRTK.MITMKTBR                          
         MVC   SRTK.MITMKTBR,SPACES                                             
*        MVC   SORTKEY,MITKEY                                                   
         B     INPPOST                                                          
         DROP  SRTK                                                             
*                                                                               
INP99    CLC   MITSEQ,=C'99'       AUTHORIZATION RECDS                          
         BE    INPPURG                                                          
*                                                                               
INPXX    DC    H'0'                UNKNOWN RECD TYPE                            
*                                                                               
INPPOST  DS    0H                  POST RECORD TO SORTER                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         B     INPLP               GO PICK UP NEXT RECD FROM TAPE               
*                                                                               
INPPURG  DS    0H                  BYPASS POSTING THIS RECD TO SORTER           
         B     INPLP               GO PICK UP NEXT RECD FROM TAPE               
*                                                                               
INPCLOSE CLOSE (FILIN,)                                                         
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* GETHH - COMPUTES THE HALF HR # FROM THE HOUR/MINUTE PROVIDED IN KEY.          
*         NEEDED TO KEEP THE RECORDS IN SEQUENCE                                
GETHH    ST    RE,SAVERE                                                        
         XC    DUB,DUB             HH NOT GIVEN. COMPUTE FROM TIME              
         PACK  DUB,MITHOUR(4)      HOUR+MINUTE (MILITARY)                       
         CVB   R1,DUB              BINARY MINUTES                               
         CHI   R1,2400                                                          
         BNH   *+8                                                              
         SHI   R1,2400                                                          
         STCM  R1,3,HALF                                                        
         GOTO1 =V(HRTOQH),DMCB,HALF,BYTE                                        
         ZIC   R3,BYTE                                                          
         LA    R3,1(R3)                                                         
         STC   R3,BYTE             QH NUMBER (STARTS AT 01)                     
         LA    R3,1(R3)            HH = (QH+1)/2 (STARTS AT 01)                 
         SRL   R3,1                                                             
         STC   R3,TMP                                                           
         EDIT  (1,TMP),(2,MITHLFID)                                             
         OC    MITHLFID,ZEROS      PADD WITH ZEROS, NOT SPACES                  
*        EDIT  BYTE,(2,MITQTRID)                                                
*        OC    MITQTRID,ZEROS      PADD WITH ZEROS, NOT SPACES                  
         MVC   SORTKEY,MITKEY                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
**********************************************************************          
* MERGE - GET RECDS BACK FROM SORTER.  PROCESS DELETE/CHANGE LOGIC.             
* COMBINE D'S WITH P'S AND H'S WITH P'S                                         
**********************************************************************          
*                                                                               
MERGE    NTR1                                                                   
         OPEN  (FILOUT,(OUTPUT))                                                
         OPEN  (FILOUT2,(OUTPUT))                                               
         XC    SVKEY,SVKEY                                                      
         XC    SVKEY1,SVKEY1                                                    
         MVI   REL,C'N'                                                         
         XC    DELKEY,DELKEY                                                    
         L     R0,AREC             CLEAR BUFFER- BLANK PAD                      
         LHI   R1,RECLENQ                                                       
         LA    RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                                                               
MRG15    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    MRG20                                                            
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
         USING MITCND,R2                                                        
*                                                                               
         CLC   MITSEQ,=C'05'                                                    
         BNE   MRG17               RESTORE SORTKEY FOR '05'                     
         MVC   MITMKTBR,MITQTRID                                                
         MVC   MITQTRID,SPACES                                                  
         MVC   MITREC,MITSEC                                                    
         MVC   MITSEC,ZEROS                                                     
         DROP  R2                                                               
*                                                                               
MRG17    BAS   RE,COMB                                                          
         MVC   PRVSRTK,SORTKEY     PREV RECDS SORTKEY                           
         B     MRG15               GO GET ANOTHER RECD                          
*                                                                               
MRG20    DS    0H                  DONE PROCESSING SORTED TAPE RECDS            
         CLI   REL,C'Y'                                                         
         BNE   MRGX                                                             
         L     R3,AREC                                                          
         CLI   MITORIG-MITCND(R3),C'X' DELETE THIS RECD?                        
         BE    MRG30                                                            
*                                                                               
MRG25    DS    0H                                                               
OUTR     USING MITCND,R3                                                        
         MVC   OUTR.MITKEY+760(1),OPTIONL     OPTIONAL BYTE                     
         CLC   OUTR.MITCORDT,BLANKS <--REPLACE AFTER TESTING                    
         BE    *+14                                                             
         CLC   OUTR.MITCORDT,=C'1000731'                                        
         BL    MRG30               DELETE CORR RECDS < 7/31/00                  
         AP    OUTCNT,=P'1'                                                     
         PUT   FILOUT,(R3)         PUT RECD TO OUTPUT TAPE                      
         CLC   OUTR.MITCORDT,BLANKS                                             
         BE    MRG30                                                            
         AP    OUTCNT2,=P'1'                                                    
         PUT   FILOUT2,(R3)        OUTPUT CORR RECDS TO 2ND OUTPUT FILE         
*                                                                               
MRG30    DS    0H                                                               
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(35),=C'TOTAL RECORDS OUTPUT FOR FILOUT  = '                    
         EDIT  OUTCNT,(15,P+35),COMMAS=YES,ZERO=NOBLANK                         
         GOTO1 =V(PRINTER)                                                      
         MVC   P(35),=C'TOTAL RECORDS OUTPUT FOR FILOUT2 = '                    
         EDIT  OUTCNT2,(15,P+35),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MRGX     CLOSE FILOUT                                                           
         CLOSE FILOUT2                                                          
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*COMB -  COMBINE RECDS INTO ONE LONG RECD: D-P-P AND H-P-P                      
**********************************************************************          
COMB     NTR1                      COMBINE DEMO RECS W/HEADERS                  
         L     R2,DMCB+4           R2=A(SORTKEY FOLLOWED BY RECD)               
         LA    R5,SORTKEY                                                       
         USING SRTKD,R5                                                         
         MVC   SORTKEY,0(R2)                                                    
*                                                                               
         LA    R2,L'SORTKEY(R2)    ACTUAL RECORD                                
         USING MITCND,R2                                                        
         L     R4,AREC             R4->BUFFER FOR MERGED RECD BEING BLT         
*                                                                               
         CLI   MITREC,C'D'         IDENTIFY D-REC FOLLOWED BY D-REC             
         BNE   CMB5                                                             
         OC    PREVREC,PREVREC                                                  
         BZ    CMB5                                                             
         CLC   MITREC,PREVREC                                                   
         BNE   CMB5                                                             
         CLC   MITMKTBR,=C'000'    PRINT FIRST MARKET BREAK ONLY                
         BNE   CMB5                                                             
         MVC   P,0(R2)             PRINT IT                                     
         GOTO1 =V(PRINTER)                                                      
CMB5     MVC   PREVREC,MITREC      SAVE RECORD TYPE (D,P,H)                     
*                                                                               
TST      DS    0H                                                               
*        L     R2,DMCB+4                                                        
*        B     CMB70                                                            
*                                                                               
AK       USING MITCND,R4                                                        
DK       USING SRTKD,DELKEY                                                     
PRV      USING SRTKD,PRVSRTK                                                    
*                                                                               
         CLC   SRTSEQ,=C'04'                                                    
         BE    CMB10                                                            
         CLI   MITREC,C'P'         P-RECD MERGE?                                
         BNE   CMB70                NO, ADD THIS RECD TO BUFFER                 
*        L     R1,AREC                                                          
*        MVC   P+5,0(R1)                                                        
*        GOTO1 =V(PRINTER)                                                      
         L     RF,AREC             COMBINE P-KEYS W/RECD IN BUFFER              
         CLC   MITKEY(MITHLFID-MITCND),0(RF)  CMP TILL RECTYP FLD               
         BE    CMB_P                                                            
         MVC   WORK(L'MITKEY),MITKEY                                            
         DC    H'0'                                                             
*                                                                               
CMB10    CLI   SRTORIG,C'D'        DELETE?                                      
         BNE   CMB15                                                            
         CLC   SORTKEY(SRTORIG-SRTKEY),DELKEY   DUPLICATE DELETE RQST           
         BE    CMBX                             KEEP ALL.NEW FORMAT HAS         
*                                      MULTIPLE AVG RECDS FOR DIFF DAYS         
*        CLC   DK.SRTTELC,ZEROS    WAS THIS A TELC LEVEL DELETE?                
*        BNE   CMB12               YES, WE DON'T MATCH, KEEP RECD               
*        CLC   SORTKEY(SRTTELC-SRTKEY),DELKEY   NO,SEE IF TRK MATCHES           
*        BE    CMBX                YES, SAME PRG#/TRK --> DELETE RECD           
*        CLC   DK.SRTTRK,BLANKS    WAS THIS A TRK LEVEL DELETE?                 
*        BNE   CMB12               YES, WE DON'T MATCH, KEEP RECD               
*        CLC   SORTKEY(SRTTRK-SRTKEY),DELKEY   NO, SEE IF PRGM MATCHES          
*        BE    CMBX                YES, DELETE RECD                             
*                                                                               
CMB12    MVC   DELKEY,SORTKEY      SAVE DELETION KEY FOR FUTURE CMPS            
         CLC   SRTBK,ORIGSDT       IF NOT INBOOK DEL, SAVE DEL RECD             
         BNE   CMB70                YES, RELEASE RECD AS IS TO FILE             
         B     CMBX                   BUT DON'T SAVE THE RECD ITSELF            
*                                                                               
CMB15    OC    DELKEY,DELKEY       WAS A DELETE REQUESTED?                      
         BZ    CMB20                                                            
         CLC   SORTKEY(SRTORIG-SRTKEY),DELKEY                                   
         BE    CMBX                SAME PRG#/TRK/TELC# --> DELETE RECD          
*        CLC   DK.SRTTELC,ZEROS    WAS THIS A TELC LEVEL DELETE?                
*        BNE   CMB20               YES, WE DON'T MATCH, KEEP RECD               
*        CLC   SORTKEY(SRTTELC-SRTKEY),DELKEY   NO,SEE IF TRK MATCHES           
*        BE    CMBX                YES, SAME PRG#/TRK --> DELETE RECD           
*        CLC   DK.SRTTRK,BLANKS    WAS THIS A TRK LEVEL DELETE?                 
*        BNE   CMB20               YES, WE DON'T MATCH, KEEP RECD               
*        CLC   SORTKEY(SRTTRK-SRTKEY),DELKEY   NO, SEE IF PRGM MATCHES          
*        BE    CMBX                YES, DELETE RECD                             
         DROP  DK                                                               
*                                                                               
CMB20    OC    CHGKEY,CHGKEY       CHANGE REQUEST?                              
         BZ    CMB30                                                            
         CLI   SRTORIG,C'0'                                                     
         BNE   CMB30                                                            
         CLC   SORTKEY(SRTORIG-SRTKEY),CHGKEY  MUST BE EXACT MATCH              
         BE    CMBX                GET RID OF NEW RECD IF MATCHES               
         B     CMB50                                                            
*                                                                               
CMB30    CLI   SRTORIG,C'0'                                                     
         BE    CMB50               MERGE ON KEYS                                
         CLI   SRTORIG,C'M'        CHANGE RECD                                  
         BE    *+6                                                              
         DC    H'0'                ONLY EXPECT CHG OR ORIG                      
         CLI   MITREC,C'D'                                                      
         BNE   CMB50                                                            
         MVC   CHGKEY,SORTKEY                                                   
         B     CMB50                                                            
*                                                                               
CMB50    CLI   MITREC,C'D'                                                      
         BE    CMB70                                                            
         CLI   MITREC,C'P'                                                      
         BE    *+6                                                              
         DC    H'0'                UNEXPECTED RECD TYPE                         
         CLC   AK.MITKEY(MITREC-MITCND),0(R2)   SAME AS RECD IN BUFFER?         
         BE    CMB_P                                                            
         MVC   WORK(L'MITKEY),0(R2)                                             
         DC    H'0'                                                             
*                                                                               
CMB70    DS    0H                                                               
         CLI   REL,C'Y'            RELS LAST RECD (RECD IN AREC)                
         BNE   CMB72                                                            
OUTREC   USING MITCND,R4                                                        
         MVC   OUTREC.MITKEY+760(1),OPTIONL       OPTIONAL BYTE                 
         CLC   OUTREC.MITCORDT,BLANKS                                           
         BE    *+14                                                             
         CLC   OUTREC.MITCORDT,=C'1000731'                                      
         BL    CMB72               DELETE CORR RECDS < 7/31/00                  
         AP    OUTCNT,=P'1'                                                     
         PUT   FILOUT,(R4)         RELEASE RECD TO OUTPUT FILE                  
         CLC   MITCORDT-MITCND(7,R4),BLANKS                                     
         BE    CMB72                                                            
         AP    OUTCNT2,=P'1'                                                    
         PUT   FILOUT2,(R4)        FOR TST/DEBUG OUTPUT CORR TO FIL2            
*                                                                               
CMB72    DS    0H                                                               
         L     R0,AREC             CLEAR AREC BUFFER (BLANK PAD)                
         LHI   R1,RECLENQ                                                       
         LA    RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                  MOVE SORTER RECD INTO AREC BUFFER            
CMB80    MVI   REL,C'Y'            SET FLAG INDICATING NEW RECD TO RELS         
         L     RF,AREC                                                          
         LA    R1,400              LENGTH OF HEADER RECD                        
         LR    RE,R2                                                            
         MOVE  ((RF),(R1)),(RE)    SAVE RECD IN AREC                            
         MVC   OPTIONL,400(R2)                                                  
*                                                                               
CMBX     XIT1                                                                   
         DROP  AK                                                               
         SPACE 2                                                                
*---------------------------------------------------------------------          
*CMB_P - SLOT P-RECDS INTO APPROPRIATE POSN IN OUTPUT RECD                      
*---------------------------------------------------------------------          
CMB_P    DS    0H                                                               
         L     RF,AREC             BUILD CMBINED RECD HERE                      
         LA    R1,400              LENGTH OF HEADER RECD                        
         AR    RF,R1               PT TO 1ST POSTN AFTER HEADER                 
         LA    R1,NDEMS*9          DISP TO PUTS (TOT DEMOS*9BYTE BKTS)          
* MITPUT IS BLANK ON NAD MIT                                                    
*        CLI   MITPUT,C' '         TREAT A BLANK LIKE A '0'                     
*        BE    *+14                NO                                           
*        CLI   MITPUT,C'0'         ARE THESE DEMOS PUTS?                        
*        BE    *+6                 NO                                           
*        AR    RF,R1               YES- PT TO PUT AREA IN AREC                  
         LA    R1,MIPNDEMS*9       DISP TO DEMOS 21-40                          
         CLC   MITDEMG,=C'501'     DEMOS GROUP 21-40? (NAD)                     
         BE    CMB_P10                                                          
         CLC   MITDEMG,=C'521'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    RF,R1               YES                                          
CMB_P10  LA    RE,MIPDEM1          PT TO DEMOS IN SOURCE RECD                   
         MOVE  ((RF),(R1)),(RE)    SAVE 'P' RECD DEMOS IN AREC (RF)             
*                                                                               
CMBP_X   XIT1                                                                   
         DROP  R2                                                               
*                                                                               
NDEMS    EQU   40                  TOTAL 40 DEMOS IN THIS RECD                  
*                                                                               
*****************************************************************               
*PRTFLDS - FOR LOOKING AT DATA ON THIS FILE                                     
*****************************************************************               
PRTFLDS  NTR1                                                                   
         LA    R4,MID1                                                          
         USING PRNTD,R4                                                         
         MVC   PITTYPE,=C'NET   '                                               
         MVC   PITCOVG,=C'COVRG '                                               
         MVC   PITPRG,=C'PRG NUMBER'                                            
         MVC   PITTRACK,=C'TRK '                                                
         MVC   PITBREAK,=C'B'                                                   
         MVC   PITSPC,=C'S'                                                     
         MVC   PITCOMR,=C'C'                                                    
         MVC   PITAVG,=C'AV'                                                    
         MVC   PITTELC,=C'TELC#'                                                
         MVC   PIDPNAME,=C'PROGRAM NAME                  '                      
         MVC   PIDTKNAM,=C'TRACKAGE NAME                 '                      
         MVC   PIDEPNAM,=C'EPIS NAME   '                                        
         MVC   PIDEPNUM,=C'EPS#'                                                
         MVC   PIDDWOS,=C'DAY M-S'                                              
         MVC   PIDHOUR,=C'TIME'                                                 
         MVC   PIDDUR,=C'DUR '                                                  
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*&&DO                                                                           
* ****************************************************************              
* PRNTREC -    PRINT FIELDS FROM RECD                                           
* ****************************************************************              
PRNTREC  NTR1                                                                   
         LA    R4,P                                                             
         USING PRNTD,R4                                                         
         CLI   MITORIG,C'0'                                                     
         BNE   PRNT20                                                           
         MVC   PITTYPE,MITTYPE                                                  
         MVC   PITCOVG,MITCOVG                                                  
         MVC   PITPRG,MITPRG                                                    
         MVC   PITTRACK,MITTRACK                                                
         MVC   PITBREAK,MITBREAK                                                
         MVC   PITSPC,MITSPC                                                    
         MVC   PITCOMR,MIDCOMR                                                  
         MVC   PITAVG,MITAVG                                                    
         MVC   PITTELC,MITTELC+5                                                
         MVC   PIDPNAME,MIDPNAME                                                
         MVC   PIDTKNAM,MIDTKNAM                                                
         MVC   PIDDWOS,MIDDAYS                                                  
         MVC   PIDHOUR,MITHOUR                                                  
         MVC   PIDDUR,MIDDUR                                                    
         MVC   PIDEPNAM,MIDEPNAM                                                
         MVC   PIDEPNUM,MIDEPNUM  CLT SPECD EPISODE NUMBER                      
         B     PRNT30                                                           
*                                                                               
PRNT20   DS    0H                  DSECT FOR CORRECTION RECDS                   
         USING PRNCRT,R4                                                        
         MVC   PRNORIG,MITORIG                                                  
         MVC   PRNCORDT,MITCORDT+1                                              
         MVC   PRNCORRS,MITCORRS                                                
         MVC   PRNNET,MITTYPE                                                   
         MVC   PRNCOVG,MITCOVG                                                  
         MVC   PRNPRG,MITPRG                                                    
         MVC   PRNTRK,MITTRACK                                                  
         MVC   PRNAVG,MITAVG                                                    
         MVC   PRNTELC,MITTELC+5                                                
         MVC   PRNNAME,MIDPNAME                                                 
         MVC   PRNDAYS,MIDDAYS                                                  
         MVC   PRNSTIM,MITHOUR                                                  
         MVC   PRNST,MITSTART                                                   
         MVC   PRNEND,MITEND                                                    
         MVC   PRNDUR,MIDDUR                                                    
         MVC   PRNEPNAM,MIDEPNAM                                                
         MVC   PRNEPNUM,MIDEPNUM                                                
*                                                                               
PRNT30   DS    0H                                                               
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         EJECT                                                                  
*&&                                                                             
*                                                                               
* I/O ERROR HANDLER                                                             
*                                                                               
IOERR    DS    0H                                                               
*                                                                               
* EXTRACT SYSTEM ERROR MESSAGES INTO FIELD WORK. SEE IBM MANUAL                 
* "Z/OS DFSMS MACRO INSTRUCTIONS FOR DATA SETS", SECTION ON SYNADAF             
* MACRO, FOR DETAILS. IF WE HAVE A DDNAME, TRY TO EXTRACT A DSN OR              
* PATHNAME AND DISPLAY IT ALONG WITH THE SYNAD MESSAGES.                        
*                                                                               
         SYNADAF ACSMETH=QSAM                                                   
         MVC   WORK,50(R1)         MESSAGE AREA FROM SYNADAF                    
         SYNADRLS                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'I/O ERROR: FORCING USER AB+        
               END.'                                                            
*                                                                               
         CLC   WORK+25(8),SPACES   DO WE HAVE A DDNAME?                         
         BE    IOERR20             NO                                           
*                                  YES: TRY TO EXTRACT DSN                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRTDSN)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO DSN AVAILABLE                             
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
         CLC   =C'...PATH=.SPECIFIED...',0(RE)                                  
         BNE   IOERR10             IT'S NOT A PATHNAME                          
*                                                                               
*                                  TRY TO EXTRACT PATHNAME                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRPATH)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO PATHNAME RETURNED                         
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
*                                                                               
IOERR10  DS    0H                                                               
         MVC   OPERMSG(21),=C'FAILURE READING FROM '                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   OPERMSG+21(0),0(RE) PUT PATHNAME INTO CONSOLE MESSAGE            
         AHI   R2,1+21             R2 = L'MESSAGE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',((R2),OPERMSG)                        
*                                                                               
IOERR20  DS    0H                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'SYNAD ERROR MESSAGES FOLLO+        
               W:'                                                              
         MVC   OPERMSG,SPACES      BUILD FIRST MESSAGE LINE...                  
         MVC   OPERMSG(59),WORK+18 STARTING AFTER <JOBNAME,STEPNAME,>           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,OPERMSG)                          
         CLI   WORK+77,C'S'        IS THERE A 2ND MESSAGE?                      
         BNE   IOERRXIT                                                         
         MVC   OPERMSG,SPACES      YES: BUILD IT                                
         MVC   OPERMSG,WORK+94                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPERMSG,OPERMSG)                   
*                                                                               
IOERRXIT DS    0H                                                               
         ABEND 925                                                              
         EJECT                                                                  
SORTCRD  DC    CL80'SORT FIELDS=(1,114,BI,A) '                                  
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=515'                                   
         EJECT                                                                  
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=FB,MACRF=(GM),              X        
               EODAD=INPCLOSE,LRECL=0401,BLKSIZE=0,SYNAD=IOERR                  
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=761,BLKSIZE=07610                                          
FILOUT2  DCB   DDNAME=FILOUT2,DSORG=PS,RECFM=FB,MACRF=(PM),            X        
               LRECL=761,BLKSIZE=07610                                          
*                                                                               
CHGBUFLN DC    F'190000'                                                        
INCNT    DC    F'0'                                                             
OUTCNT   DC    PL6'0'                                                           
OUTCNT2  DC    PL6'0'                                                           
OPERMSG  DS    CL100               MAXIMUM LENGTH FOR LOGIO                     
PREVREC  DC    X'0'                                                             
DUB      DS    D                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
SAVERE   DS    A                                                                
DMCB     DS    6F                                                               
WORK     DS    CL256                                                            
TMP      DS    CL5                                                              
ORIGSDT  DS    CL7                                                              
REL      DS    X                                                                
CHAR     DS    C                                                                
         DC    C'***AREC***'                                                    
AREC     DS    F                                                                
BLANKS   DC    CL255' '                                                         
ZEROS    DC    40C'0'                                                           
OPTIONL  DS    C                                                                
PRCHGKEY DS    CL36                                                             
SVKEY    DS    CL26                BACK UP SEQ/PRG/TRK/TEL/ORIGIN               
SVKEY1   DS    CL26                                                             
DELKEY   DS    CL(L'MITKEY)                                                     
CHGKEY   DS    CL(L'MITKEY)                                                     
PRVSRTK  DS    CL114      SORT KEY SORTKEY OF LAST RECD FROM SORTER             
         DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
SORTKEY  DS    CL114      SORT KEY KEY FOR SEQUENCING DATA ON 04 RECDS          
*                                                                               
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
*                                                                               
LASTREC  DS    CL(RECLENQ)                                                      
*                                                                               
CHGBUFF  DS    190000C                                                          
         EJECT                                                                  
SRTKD    DSECT                                                                  
SRTKEY   DS    0CL114                                                           
SRTSEQ   DS    CL2                 MITSEQ                                       
SRTBK    DS    CL7                 CORRECTION BOOK OR TAPE BK                   
SRTSTA   DS    CL6                 MITCOVG                                      
SRTPRG   DS    CL10                MITPRG                                       
SRTTRK   DS    CL3                 MITTRACK                                     
SRTTELC  DS    CL10                MITTELC                                      
SRTORIG  DS    CL1                 MITORIG                                      
SRTCTR   DS    XL4                 SEQ CTR FROM SEQ ON INPUT TAPE               
SRTRSN   DS    CL3                 CORRECTION REASON FIELD                      
*                                                                               
PRNTD    DSECT                                                                  
PITTYPE  DS    CL6       15-20     DATA TYPE CODE                               
PITCOVG  DS    CL6       71-76     COVERAGE SAMPLE ID                           
         DS    C                                                                
PITPRG   DS    CL10      21-30     PRG CODE/CAB NET/STN ID/STN GRP              
         DS    C                                                                
PITTRACK DS    CL3       31-33     TRACKAGE ID                                  
         DS    C                                                                
PITBREAK DS    CL1       35        BREAKOUT IND                                 
PITSPC   DS    CL1       36        SPECIAL IND                                  
PITCOMR  DS    CL1       36        COMMERCIAL STATUS MIDCOMR                    
         DS    C                                                                
PITAVG   DS    CL2       42-43     NUMBER OF DAYS/WEEKS IN AVG                  
         DS    C                                                                
PITTELC  DS    CL5       58-67     TELECAST NUMBER                              
         DS    C                                                                
PIDPNAME DS    CL25      115-139   PROGRAM NAME                                 
         DS    C                                                                
PIDTKNAM DS    CL25      140-164   TRACKAGE NAME                                
         DS    C                                                                
PIDDWOS  DS    CL7                 DAYS OF WEEK BITS                            
         DS    C                                                                
PIDHOUR  DS    CL4                 START TIME                                   
         DS    C                                                                
PIDDUR   DS    CL4                 DURATION                                     
         DS    C                                                                
PIDEPNAM DS    CL10       180-211  EPISODE NAME                                 
         DS    C                                                                
PIDEPNUM DS    CL4        212-215  EPISODE NUMBER                               
         DS    C                                                                
*                                                                               
PRNCRT   DSECT                     DSECT FOR CORRECTION RECDS                   
PRNORIG  DS    CL1                 CORRECTION TYPE                              
         DS    C                                                                
PRNCORDT DS    CL6                 CORRECTION START DATE                        
         DS    C                                                                
PRNCORRS DS    CL3                 CORRECTION REASON                            
         DS    C                                                                
PRNNET   DS    CL6                 NET IF PRESENT                               
         DS    C                                                                
PRNCOVG  DS    CL6                 COVERAGE SAMPLE ID                           
         DS    C                                                                
PRNAVG   DS    CL2                 NUMBER DAYS IN AVG                           
         DS    C                                                                
PRNPRG   DS    CL10                PROGRAM NUMBER                               
         DS    C                                                                
PRNTRK   DS    CL3                 TRACKAGE                                     
         DS    C                                                                
PRNTELC  DS    CL5                 TELECAST NUMBER                              
         DS    C                                                                
PRNST    DS    CL6                 START DATE                                   
         DS    C                                                                
PRNEND   DS    CL6                 END DATE                                     
         DS    C                                                                
PRNSTIM  DS    CL4                 START TIME                                   
         DS    C                                                                
PRNDAYS  DS    CL7                 DAYS                                         
         DS    C                                                                
PRNNAME  DS    CL25                PROGRAM NAME                                 
         DS    C                                                                
PRNDUR   DS    CL4                 DURATION                                     
         DS    C                                                                
PRNEPNAM DS    CL10       180-211  EPISODE NAME                                 
         DS    C                                                                
PRNEPNUM DS    CL4        212-215  EPISODE NUMBER                               
         DS    C                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DEMITCND                                                       
         EJECT                                                                  
         IEFZB4D2                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080DECNSOR4  11/22/13'                                      
         END                                                                    
