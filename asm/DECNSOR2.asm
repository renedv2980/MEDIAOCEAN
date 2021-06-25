*          DATA SET DECNSOR2   AT LEVEL 081 AS OF 02/02/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNSR2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DDINFO                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'SORT CABLE DEMO TAPE BEFORE RUNNING CONVERSION PRGM'            
DECABSRT CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,DECABSRT,=V(REGSAVE),R8                                        
         USING MITD,R2              R2 = MIT DSECT                              
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         MVC   TITLE,=CL60'CABLE TAPE PRE-CONVERSION SORT'                      
*                                                                               
GETCARD  DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'                                     
         CLC   =C'/*',CARD                                                      
         BE    CHKVAL                                                           
*                                                                               
         MVC   P(L'CARD),CARD      PRINT EACH PARAMETER CARD                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    GETCARD             YES: IGNORE IT                               
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     GETCARD                                                          
*                                                                               
         CLC   =C'PANLIB=',CARD    DEFAULT IS 'PAN.APPL.LIBRARY'                
         BNE   *+14                                                             
         MVC   PANLIB,CARD+7       OVERRIDE PANLIB FOR ICETOOL SOURCE           
         B     GETCARD                                                          
*                                                                               
         CLC   =C'WARN=',CARD      DEFAULT IS TO ISSUE EMAIL WARNING            
         BNE   *+14                                                             
         MVC   WARNFLAG,CARD+5     BETTER BE YES/NO                             
         B     GETCARD                                                          
*                                                                               
         CLC   =C'PREVALIDATE=',CARD                                            
         JNE   *+2                 UNKNOWN PARAMETER CARD                       
         MVC   PREVAL,CARD+12      BETTER BE YES/NO                             
         B     GETCARD                                                          
*                                                                               
CHKVAL   DS    0H                                                               
         CLI   PREVAL,C'Y'         PERFORM PRE-VALIDATION?                      
         BNE   INIT                NO                                           
*                                                                               
* DEIS FEB/2021:                                                                
*    NIELSEN OCCASIONALLY SENDS US MIT FILES CONTAINING ERRORS, E.G.,:          
*     1. "04"/"P" CORRECTION RECORDS WITHOUT AN ASSOCIATED "D" RECORD           
*     2. "06"/"P" RECORDS WITHOUT AN ASSOCIATED "C" RECORD                      
*    THIS PROGRAM CHOKES ON THESE UNEXPECTED CONDITIONS.                        
*    TO GET AROUND THIS, WE SUPPORT A "PREVALIDATE=Y" CONTROL CARD.             
*    WE USE ICETOOL TO ANALYZE THE ORIGINAL "FILIN" DATASET, AND THEN           
*    WE PRODUCE A TEMPORARY "MITOUT" DATASET FROM WHICH ALL UNMATCHED           
*    RECORDS HAVE BEEN *REMOVED*. THE "FILIN" DCB IS MODIFIED TO                
*    REFERENCE "MITOUT", SO THAT ALL DOWNSTREAM CODE IS TRANSPARENT.            
*    ALL REQUIRED DATASETS ARE DYNAMICALLY ALLOCATED, SO THAT WE DON'T          
*    NEED ANY NEW DD STATEMENTS TO SUPPORT THIS ICETOOL INVOCATION.             
*    SEE THESE PANVALET MEMBERS FOR DETAILS:                                    
*      DEDFCNSRTI (TOOLIN)                                                      
*      DEDFCNSRSY (SYMNAMES)                                                    
*      DEDFCNSRC1 (COP1CNTL)                                                    
*      DEDFCNSRJ1 (JON1CNTL)                                                    
*      DEDFCNSRJ3 (JON3CNTL)                                                    
*      DEDFCNSRJ4 (JON4CNTL)                                                    
*      DEDFCNSRS1 (SRT1CNTL)                                                    
*                                                                               
         LA    RF,FILIN            A(MIT FILE INPUT DCB)                        
         MVC   DCBDDNAM-IHADCB(,RF),=CL8'MITOUT'  DDNAME=MITOUT                 
*                                                                               
* DYNAMICALLY ALLOCATE DATASETS NEEDED BY ICETOOL.                              
*  NOTE: ALL TEMPORARY DATASETS ARE ALLOCATED WITH                              
*        STORCLAS=SCTEMP,DATACLAS=DCTEMP                                        
*                                                                               
* //MITOUT   DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //T1       DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=MOD         
* //MISSING  DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=MOD         
* //TYPE0406 DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //TYPE04D  DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //TYPE04P  DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //TYPE06C  DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //TYPE06P  DD UNIT=SYSDA,SPACE=(CYL,(100,100)),DCB=(BUFNO=2),DISP=NEW         
* //SYMNAMES DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRSY),DISP=SHR         
* //TOOLIN   DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRTI),DISP=SHR         
* //COP1CNTL DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRC1),DISP=SHR         
* //JON1CNTL DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRJ1),DISP=SHR         
* //JON3CNTL DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRJ3),DISP=SHR         
* //JON4CNTL DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRJ4),DISP=SHR         
* //SRT1CNTL DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,DEDFCNSRS1),DISP=SHR         
* //DFSMSG   DD SYSOUT=*                                                        
* //TOOLMSG  DD SYSOUT=*                                                        
* //SHOWBAD  DD SYSOUT=*                                                        
* //SYMNOUT  DD SYSOUT=*                                                        
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'MITOUT'),                 +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'T1'),                     +        
               (X'48',=AL3(100,100)),0,0,(X'80',A(TXTUNTS1)) *DISP=MOD          
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'MISSING'),                +        
               (X'48',=AL3(100,100)),0,0,(X'80',A(TXTUNTS1)) *DISP=MOD          
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'TYPE0406'),               +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'TYPE04D'),                +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'TYPE04P'),                +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'TYPE06C'),                +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(X'80',=CL8'TYPE06P'),                +        
               (X'48',=AL3(100,100)),0,0,A(TXTUNTS1)                            
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'SYMNAMES'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRSY'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'TOOLIN'),                  +        
               (0,PANLIB),(0,=CL10'DEDFCNSRTI'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'COP1CNTL'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRC1'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'JON1CNTL'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRJ1'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'JON3CNTL'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRJ3'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'JON4CNTL'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRJ4'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(C'P',=CL8'SRT1CNTL'),                +        
               (0,PANLIB),(0,=CL10'DEDFCNSRS1'),0,0                             
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'DFSMSG'),(X'80',SPACES)            
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'TOOLMSG'),(X'80',SPACES)           
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'SHOWBAD'),(X'80',SPACES)           
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'SYMNOUT'),(X'80',SPACES)           
*                                                                               
         SR    R1,R1               CONTROL CARDS ARE IN TOOLIN                  
         LINK  EP=ICETOOL          PRE-VALIDATE                                 
         CHI   RF,4                RF CONTAINS THE RETURN CODE                  
         JH    *+2                 RC > 4: BAD RETURN FROM ICETOOL!             
         JNE   INIT                RC = 0: NO PROBLEMS FOUND.                   
*                                  RC = 4: ERRORS FOUND. SEND EMAIL.            
*                                                                               
         IF (CLI,WARNFLAG,EQ,C'Y')                                              
           GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('WARNMS1Q',WARNMSG1)               
         ENDIF ,                                                                
*                                                                               
INIT     OPEN  (FILIN,(INPUT))                                                  
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
         XC    INCNT,INCNT                                                      
         XC    SVKEY,SVKEY         SAVED PRG/TRK/TEL/ORIGIN                     
         LAY   RE,IOAREA                                                        
         LHI   R1,RECLENQ                                                       
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
         LAY   RE,CHGBUFF                                                       
         L     RF,CHGBUFLN                                                      
         XCEF                                                                   
         BAS   RE,INPTOP           RD TAPE,BLD SORTKEY,POST TO SORTER           
         BAS   RE,MERGE            RD AND MERGE/DELETE RECS FROM SORT           
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*INPTOP - PICK UP RECDS FROM TAPE GET RID OF UNWANTED RECDS,                    
*  POST RECORDS TO SORTER PREFIXED WITH SORTKEY.                                
*  SORTKEY=MITKEY EXCEPT FOR 04/06 RECDS WHICH HAVE A SPECIAL SORTKEY           
***********************************************************************         
INPTOP   NTR1                                                                   
INPLP    LA    R5,SORTKEY                                                       
         USING SRTKD,R5                                                         
         LAY   R2,IOAREA                                                        
         GET   FILIN,(R2)                                                       
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         USING MITD,R2             R2 = MIT DSECT                               
         MVC   SORTKEY,MITKEY      DEFAULT SORTKEY=ORIG KEY ON RECD             
*                                                                               
         CLC   PREVSEQ,MITSEQ      RESET CHANGE BUFFER FOR NEW SEQ              
         BE    INP00               (NEEDED FOR COMMERCIAL RECDS)                
         LAY   RE,CHGBUFF                                                       
         L     RF,CHGBUFLN                                                      
         XCEF                                                                   
         MVC   PREVSEQ,MITSEQ                                                   
*                                                                               
INP00    CLC   MITSEQ,=C'00'       PROGRAM DESCRIPTOR RECD                      
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
         MVC   MITPRG+4(6),MITCOVG NEED CVG HI IN KEY FOR SORT                  
         MVC   MITPRG-MITD+4(6,R5),MITCOVG                                      
         CLC   MITAVG,=C'05'       GET RID OF M-F,M-SU AVGS ON 03,05'S          
         BE    INPPURG                                                          
         CLC   MITAVG,=C'07'                                                    
         BE    INPPURG                                                          
         B     INPPOST                                                          
*                                                                               
INP04    DS    0H                  PRG RECD HANDLING                            
         CLC   MITSEQ,=C'04'                                                    
         BE    *+14                                                             
         CLC   MITSEQ,=C'06'                                                    
         BNE   INP05                                                            
         CLI   MITREC,C'E'         EXCLUDE THIS RECORD                          
         BE    INPPURG                                                          
*        CLC   MITCOVG,=C'007443' SV BYPASS                                     
*        BE    INPPURG                                                          
         CLC   MITHLFID,ZEROS      PURGE PRG 1/2HR RECDS + PRG PUTS             
         BNE   INPPURG                                                          
         CLI   MITPUT,C'1'                                                      
         BE    INPPURG                                                          
         MVC   SORTKEY,SPACES      FOR 04/06 REC, SORT BY PRG/TRK/TEL           
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
         MVC   SRTRSN,MITCORRS                                                  
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
         LAY   RE,CHGBUFF                                                       
         A     RE,CHGBUFLN                                                      
         CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'0'                CHGBUFF OVERFLOW. INCREASE SIZE              
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
         CLC   MITAVG,=C'05'       GET RID OF M-F,M-SU AVGS ON 03,05'S          
         BE    INPPURG                                                          
         CLC   MITAVG,=C'07'                                                    
         BE    INPPURG                                                          
         B     INPPOST                                                          
*                                                                               
INP99    CLC   MITSEQ,=C'99'       AUTHORIZATION RECDS                          
         BE    INPPURG                                                          
*                                                                               
INPXX    DC    H'0'                UNKNOWN RECD TYPE --> ?                      
*                                                                               
INPPOST  DS    0H                  POST RECORD TO SORTER                        
*        MVC   P,SORTKEY                                                        
*        GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         CLC   MITSEQ,=C'03'       CREATE QHR RECS FROM HLFHRS FOR              
         BE    *+14                 03 AND 05 TP RECDS                          
         CLC   MITSEQ,=C'05'                                                    
         BNE   INPLP                                                            
         CLI   MITREC,C'H'                                                      
         BNE   INPLP                                                            
         CLC   MITAVG,ZEROS        NO QTR HR AVGS RECD                          
         BNE   INPLP                                                            
         PACK  DUB,MITHLFID                                                     
         CVB   R3,DUB                                                           
         SLL   R3,1                HLF HR CODE * 2  - 1 = QTR HR CODE           
         BCTR  R3,0                                                             
         MVI   TMP+1,0             OUTPUT 2 QTR HR RECDS                        
INPPOST5 STC   R3,TMP                                                           
         EDIT  (1,TMP),(2,MITQTRID)                                             
         OC    MITQTRID,ZEROS      PAD WITH 0'S NOT BLANKS                      
         MVC   MIHDUR,=C'0015'     15-MINUTE DURATION                           
         MVC   SORTKEY,MITKEY                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTKEY                                  
         IC    RE,TMP+1                                                         
         LA    RE,1(RE)                                                         
         STC   RE,TMP+1                                                         
         LA    R3,1(R3)                                                         
         CLC   MITMIN,ZEROS                                                     
         BNE   *+10                                                             
         MVC   MITMIN,=C'15'                                                    
         CLC   MITMIN,=C'30'                                                    
         BNE   *+10                                                             
         MVC   MITMIN,=C'45'                                                    
         CLI   TMP+1,2                                                          
         BL    INPPOST5            OUTPUT 2 RECORDS                             
         B     INPLP               DONE DUPLICATING RECORD                      
*                                                                               
INPPURG  DS    0H                  BYPASS POSTING THIS RECD TO SORTER           
         B     INPLP               GO PICK UP NEXT RECD FROM TAPE               
*                                                                               
INPCLOSE CLOSE (FILIN,)                                                         
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* MERGE - GET RECDS BACK FROM SORTER.  PROCESS DELETE/CHANGE LOGIC.             
* COMBINE D'S AND C'S WITH P'S AND H'S WITH P'S                                 
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
         LAY   RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                                                               
MRG15    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    MRG20                                                            
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'SORT RECORD',(R2),C'DUMP',514,=C'2D'          
*                                                                               
*        MVC   P,0(R2)                                                          
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,COMB                                                          
         MVC   PRVSRTK,SORTKEY     PREV RECDS SORTKEY                           
         B     MRG15               GO GET ANOTHER RECD                          
*                                                                               
MRG20    DS    0H                  DONE PROCESSING SORTED TAPE RECDS            
         CLI   REL,C'Y'                                                         
         BNE   MRGX                                                             
         L     R3,AREC                                                          
         CLI   MITORIG-MITD(R3),C'X'  DELETE THIS RECD?                         
         BE    MRG30                                                            
*                                                                               
MRG25    DS    0H                                                               
         CLC   MITCORDT-MITD(7,R3),BLANKS  <--REPLACE AFTER TESTING             
         BE    *+14                                                             
         CLC   MITCORDT-MITD(7,R3),=C'1000731'                                  
         BL    MRG30               DELETE CORR RECDS < 7/31/00                  
                                                                                
         AP    OUTCNT,=P'1'                                                     
         PUT   FILOUT,(R3)         PUT RECD TO OUTPUT TAPE                      
         CLC   MITCORDT-MITD(7,R3),BLANKS                                       
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
*COMB -  COMBINE RECDS INTO ONE LONG RECD: D-P-P(C-P-P) AND H-P-P               
*                                                                               
* ON ENTRY, R2 MUST POINT TO RETURNED SORT RECORD                               
*                                                                               
**********************************************************************          
COMB     NTR1  ,                   COMBINE DEMO RECS W/HEADERS                  
         LA    R5,SORTKEY                                                       
         USING SRTKD,R5                                                         
         MVC   SORTKEY,0(R2)                                                    
         LA    R2,L'SORTKEY(R2)    ACTUAL RECORD                                
         USING MITD,R2                                                          
         L     R4,AREC             R4->BUFFER FOR MERGED RECD BEING BLT         
*                                                                               
TST      DS    0H                                                               
*        L     R2,DMCB+4                                                        
*        B     CMB70                                                            
*                                                                               
AK       USING MITD,R4                                                          
**DK       USING SRTKD,DELKEY                                                   
**PRV      USING SRTKD,PRVSRTK                                                  
*                                                                               
         CLC   SRTSEQ,=C'04'                                                    
         BE    CMB10                                                            
         CLC   SRTSEQ,=C'06'                                                    
         BE    CMB10                                                            
         CLI   MITREC,C'P'         P-RECD MERGE?                                
         BNE   CMB70                NO, ADD THIS RECD TO BUFFER                 
         L     RF,AREC             COMBINE P-KEYS W/RECD IN BUFFER              
         CLC   MITKEY(MITHLFID-MITD),0(RF)   CMP TILL RECTYP FLD                
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
*********DROP  DK                                                               
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
         BE    *+12                                                             
         CLI   MITREC,C'C'         SAME AS 'D' FOR COMMERCIAL AVERAGE           
         BNE   CMB50                                                            
         MVC   CHGKEY,SORTKEY                                                   
         B     CMB50                                                            
*                                                                               
CMB50    CLI   MITREC,C'D'                                                      
         BE    CMB70                                                            
         CLI   MITREC,C'C'         SAME AS 'D' FOR COMMERCIAL AVERAGE           
         BE    CMB70                                                            
         CLI   MITREC,C'P'                                                      
         BE    *+6                                                              
         DC    H'0'                UNEXPECTED RECD TYPE                         
         CLC   AK.MITKEY(MITREC-MITD),0(R2)    SAME AS RECD IN BUFFER?          
         BE    CMB_P                                                            
         MVC   WORK(L'MITKEY),0(R2)                                             
         DC    H'0'                *POSSIBLE* BAD INPUT FILE. CONSIDER          
*                                    RE-TRYING WITH PREVALIDATE=Y .             
*                                                                               
CMB70    DS    0H                                                               
         CLI   REL,C'Y'            RELS LAST RECD (RECD IN AREC)                
         BNE   CMB72                                                            
         CLC   MITCORDT-MITD(7,R4),BLANKS                                       
         BE    *+14                                                             
         CLC   MITCORDT-MITD(7,R4),=C'1000731'                                  
         BL    CMB72               DELETE CORR RECDS < 7/31/00                  
         AP    OUTCNT,=P'1'                                                     
         PUT   FILOUT,(R4)         RELEASE RECD TO OUTPUT FILE                  
         CLC   MITCORDT-MITD(7,R4),BLANKS                                       
         BE    CMB72                                                            
         AP    OUTCNT2,=P'1'                                                    
         PUT   FILOUT2,(R4)        FOR TST/DEBUG OUTPUT CORR TO FIL2            
*                                                                               
CMB72    DS    0H                                                               
         L     R0,AREC             CLEAR AREC BUFFER (BLANK PAD)                
         LHI   R1,RECLENQ                                                       
         LAY   RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE               MOVE SORTER RECD INTO AREC BUFFER            
*                                                                               
CMB80    MVI   REL,C'Y'            SET FLAG INDICATING NEW RECD TO RELS         
         L     RF,AREC                                                          
         LA    R1,400              LENGTH OF HEADER RECD                        
         LR    RE,R2                                                            
         MOVE  ((RF),(R1)),(RE)    SAVE RECD IN AREC                            
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
         CLI   MITPUT,C' '         TREAT A BLANK LIKE A '0'                     
         BE    *+14                NO                                           
         CLI   MITPUT,C'0'         ARE THESE DEMOS PUTS?                        
         BE    *+6                 NO                                           
         AR    RF,R1               YES- PT TO PUT AREA IN AREC                  
         LA    R1,MIPNDEMS*9       DISP TO DEMOS 21-40                          
         CLC   MITDEMG,=C'001'     ARE THESE DEMOS GROUP 21-40?                 
         BE    *+6                 NO                                           
         AR    RF,R1               YES                                          
         LA    RE,MIPDEM1          PT TO DEMOS IN SOURCE RECD                   
         MOVE  ((RF),(R1)),(RE)    SAVE 'P' RECD DEMOS IN AREC (RF)             
*                                                                               
CMBP_X   XIT1                                                                   
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
* SORT RECORD LENGTH = ORIGINAL LRECL=400 + SORTKEY=114                         
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=514'                                   
*                                                                               
WARNMSG1 DC    C'AUTONOTE*US-MFDEMOSPROGRAMMERS:ERROR(S) IN CABLE MIT F+        
               ILE. PROCESSING WILL CONTINUE: SEE OUTPUT SHOWBAD'               
WARNMS1Q EQU   *-WARNMSG1                                                       
*                                                                               
         EJECT                                                                  
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=FB,MACRF=(GM),              X        
               EODAD=INPCLOSE,LRECL=0400,BLKSIZE=0,SYNAD=IOERR                  
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=1120,BLKSIZE=11200                                         
FILOUT2  DCB   DDNAME=FILOUT2,DSORG=PS,RECFM=FB,MACRF=(PM),            X        
               LRECL=1120,BLKSIZE=11200                                         
*                                                                               
*                                                                               
CHGBUFLN DC    A(CHGBUFFL)                                                      
PREVSEQ  DC    C'  '                                                            
INCNT    DC    F'0'                                                             
OUTCNT   DC    PL6'0'                                                           
OUTCNT2  DC    PL6'0'                                                           
PREVAL   DC    C'N'                ASSUME NO ICETOOL PRE-VALIDATION             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
WORK     DS    CL256                                                            
TMP      DS    CL5                                                              
ORIGSDT  DS    CL7                                                              
REL      DS    X                                                                
CHAR     DS    C                                                                
AREC     DS    F                                                                
BLANKS   DC    CL255' '                                                         
WARNFLAG DC    C'Y'                DEFAULT IS TO ISSUE WARNING                  
PANLIB   DC    CL44'PAN.APPL.LIBRARY'  OVERRIDE PANVALET LIBRARY                
ZEROS    DC    80C'0'                                                           
OPERMSG  DS    CL100               MAXIMUM LENGTH FOR LOGIO                     
PRCHGKEY DS    CL36                                                             
SVKEY    DS    CL26                BACK UP SEQ/PRG/TRK/TEL/ORIGIN               
SVKEY1   DS    CL26                                                             
DELKEY   DS    CL(L'MITKEY)                                                     
CHGKEY   DS    CL(L'MITKEY)                                                     
PRVSRTK  DS    CL114      SORT KEY SORTKEY OF LAST RECD FROM SORTER             
         DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
TXTUNTS1 DS    0A                  ADDITIONAL DYNALLOC TEXT UNITS               
         DC    A(TEXT_UNIT_DACL)   DATA CLASS                                   
         DC    A(TEXT_UNIT_STCL)   STORAGE CLASS                                
         DC    A(TEXT_UNIT_RECFM)  RECFM                                        
         ORG   *-4                                                              
         DC    X'80'               EOL                                          
         ORG                                                                    
*                                                                               
TEXT_UNIT_STCL   DC    AL2(DALSTCL),AL2(1),AL2(L'TXTSTCL)                       
TXTSTCL          DC    C'SCTEMP'    STORCLAS=SCTEMP                             
TEXT_UNIT_DACL   DC    AL2(DALDACL),AL2(1),AL2(L'TXTDACL)                       
TXTDACL          DC    C'DCTEMP'    DATACLAS=DCTEMP                             
TEXT_UNIT_RECFM  DC    AL2(DALRECFM),AL2(1),AL2(1),AL2(L'TXTDACL)               
                 DC    X'90'        RECFM=FB                                    
         SPACE 2                                                                
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SORTKEY'                                                    
SORTKEY  DS    CL114      SORT KEY FOR SEQUENCING DATA ON 04/06 RECDS           
*                                                                               
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
*                                                                               
CHGBUFF  DS    40000CL(L'PRCHGKEY)                                              
CHGBUFFL EQU   *-CHGBUFF                                                        
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
SRTRSN   DS    CL3                 CORRECTION REASON FIELD                      
SRTCTR   DS    XL4                 SEQ CTR FROM SEQ ON INPUT TAPE               
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
       ++INCLUDE DEMITD                                                         
         EJECT                                                                  
         IEFZB4D2                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081DECNSOR2  02/02/21'                                      
         END                                                                    
