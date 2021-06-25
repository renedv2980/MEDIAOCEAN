*          DATA SET DDREVIEW   AT LEVEL 002 AS OF 09/21/11                      
*PHASE REVIEWA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'REVIEW - VIEW RECOVERY FILE STATISTICS'                         
REVIEW   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(24),=C'RECOVERY FILE STATISTICS'                           
         DATE  TODAY                                                            
         LA    R5,EXPARMS                                                       
         EXTRACT (5),'S',FIELDS=(ALL,TSO,COMM,PSB,TJID)                         
         LA    R5,EXPARMS                                                       
         DC    H'00'                                                            
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,PROCREC          PROCESS RECOVERY FILE                        
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         MVI   INTAPE,C'N'                                                      
         MVI   TDYONLY,C'N'                                                     
         MVI   RETCODE,X'FF'                                                    
         ZAP   FILEMAX,=PL4'0'                                                  
         ZAP   TRANSMAX,=PL4'0'                                                 
         ZAP   SUPPMAX,=PL4'0'                                                  
         ZAP   OTHERMAX,=PL4'0'                                                 
         ZAP   BUCKMAX,=PL4'0'                                                  
         L     RF,=A(TRKBUFF)                                                   
         ST    RF,ATRKBUFF                                                      
         L     RF,=A(SYSTAB)                                                    
         ST    RF,ASYSTAB                                                       
         ST    RF,ASYSTABL                                                      
         L     RF,=A(SYSTABX)                                                   
         ST    RF,ASYSTABX                                                      
         L     RF,=A(TIMTAB)                                                    
         ST    RF,ATIMTAB                                                       
         ST    RF,ATIMTABL                                                      
         L     RF,=A(TIMTABX)                                                   
         ST    RF,ATIMTABX                                                      
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IOL                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   P(2),=C'/*'                                                      
         BE    VCEND               END OF INPUT PARAM CARDS                     
*                                                                               
VCLP1A   CLC   P(6),=C'DDSIO='     DDSIO=XXX... TO SET THE DDSIO                
         BNE   VCLP1B                                                           
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),P                                                        
         B     VCLP1                                                            
*                                                                               
VCLP1B   CLC   P(7),=C'DSPACE='    DSPACE=X TO SET THE DATA SPACE               
         BNE   VCLP1C                                                           
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     VCLP1                                                            
*                                                                               
VCLP1C   LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VCSYSTEM                                                         
         B     VCAGENCY                                                         
         B     VCMEDIA                                                          
         B     VCTIME                                                           
         B     VCINPUT                                                          
         B     VCDATE                                                           
         B     VCTODAY                                                          
         B     VCFILE                                                           
         B     VCTRANS                                                          
         B     VCSUPP                                                           
         B     VCOTHER                                                          
         B     VCCMPANY                                                         
         B     VCBUCKET                                                         
*                                  EXIT/ERROR CONDITIONS                        
*                                  CHECK REQUIRED INPUT                         
VCEND    CLC   ASYSTAB,ASYSTABL    CHECK SYSTEM ENTERED                         
         BE    VCERR2                                                           
         B     VCOK                                                             
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 VPRINTER            INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR2   GOTO1 VPRINTER            MISSING SYSTEM DEFINITION                    
         MVI   ERROR,2                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR3   GOTO1 VPRINTER                                                         
         MVI   ERROR,3                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR4   GOTO1 VPRINTER                                                         
         MVI   ERROR,4                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR5   GOTO1 VPRINTER                                                         
         MVI   ERROR,5                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCOK     B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
VCSYSTEM EQU   *                   SYSTEM=                                      
         LA    R4,P+7                                                           
         SR    RF,RF                                                            
*                                                                               
VCSYS010 CLI   0(R4),C' '                                                       
         BE    VCSYS012                                                         
         CLI   0(R4),C'='                                                       
         BE    VCSYS012                                                         
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         B     VCSYS010                                                         
*                                                                               
VCSYS012 LTR   RF,RF                                                            
         BZ    VCERR3                                                           
         BCTR  RF,0                                                             
         LA    R3,SYSLST                                                        
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VCSYS020 CLI   0(R3),0             TEST E-O-T                                   
         BE    VCERR3                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VCSYS022                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),P+7                                                  
         BE    VCSYS030                                                         
VCSYS022 LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VCSYS020                                                         
*                                                                               
VCSYS030 MVC   SYSTEM,SYSLNUM      RETURN SYSTEM NUMBERS                        
*                                                                               
         CLI   0(R4),C'='                                                       
         BE    VCSYS100                                                         
         CLI   0(R4),C' '                                                       
         BNE   VCERR4                                                           
         B     VCSYS110                                                         
*                                                                               
VCSYS100 EQU   *                                                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    VCLP1                                                            
*                                                                               
VCSYS110 GOTO1 BLDSYS,DMCB,0(R4)                                                
         BNE   VCERR5                                                           
         L     R1,0(R1)                                                         
         LA    R1,0(R1)                                                         
         L     RF,ASYSTABL                                                      
         MVC   0(10,RF),0(R1)                                                   
         LA    RF,10(RF)                                                        
         C     RF,ASYSTABX                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         ST    RF,ASYSTABL                                                      
         B     VCSYS100                                                         
         EJECT                                                                  
VCAGENCY EQU   *                   AGENCY=                                      
         GOTO1 VNUMVAL,PARM,P+7,(X'02',0)                                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     R1,4(R1)                                                         
         C     R1,=F'15'                                                        
         BH    VCERR1                                                           
         C     R1,=F'1'                                                         
         BL    VCERR1                                                           
         STC   R1,AGYFILT                                                       
         B     VCLP1                                                            
                                                                                
         EJECT                                                                  
VCCMPANY EQU   *                   COMPANY=                                     
         GOTO1 VHEXIN,PARM,P+8,CPYFILT,2,0                                      
         CLC   12(4,R1),=F'1'                                                   
         BNE   VCERR1                                                           
         B     VCLP1                                                            
                                                                                
VCMEDIA  EQU   *                   MEDIA=                                       
         CLI   P+6,C'T'                                                         
         BE    VCMED010                                                         
         CLI   P+6,C'P'                                                         
         BE    VCMED010                                                         
         B     VCERR1                                                           
VCMED010 MVC   MEDFILT,P+6                                                      
         B     VCLP1                                                            
                                                                                
VCTIME   EQU   *                   TIME=HHMM-HHMM                               
         L     RF,ASYSTAB                                                       
         C     RF,ASYSTABL                                                      
         BE    VCERR1                                                           
         GOTO1 GETSYS,DMCB,0(RF)                                                
         LA    RF,9                                                             
         GOTO1 VTIMBER,PARM,(X'80',(RF)),(X'01',FULL),P+5                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     RF,ATIMTABL                                                      
         MVC   0(4,RF),FULL                                                     
         LA    RF,4(RF)                                                         
         CLI   DMSYSNUM,X'04'                                                   
         BE    *+12                                                             
         LA    RE,AFILTAB                                                       
         B     *+8                                                              
         LA    RE,MFILTAB                                                       
*                                                                               
VCTIM010 CLI   0(RE),X'FF'                                                      
         BE    VCTIM020                                                         
         MVC   0(SFILTABL,RF),0(RE)                                             
         LA    RE,SFILTABL(RE)                                                  
         LA    RF,SFILTABL(RF)                                                  
         C     RF,ATIMTABX                                                      
         BE    VCERR1                                                           
         B     VCTIM010                                                         
*                                                                               
VCTIM020 MVI   0(RF),X'FF'                                                      
         LA    RF,1(RF)                                                         
         ST    RF,ATIMTABL                                                      
         B     VCLP1                                                            
                                                                                
VCINPUT  EQU   *                   INPUT=                                       
         CLC   P+6(4),=C'TAPE'                                                  
         BNE   VCERR1                                                           
         MVI   INTAPE,C'Y'                                                      
         B     VCLP1                                                            
                                                                                
VCDATE   EQU   *                   DATE=                                        
         GOTO1 VDATVAL,DMCB,(0,P+5),TODAY                                       
         OC    DMCB(4),DMCB                                                     
         BZ    VCERR1                                                           
         MVC   SPECDATE(5),=C'DATE='                                            
         MVC   SPECDATE+5(6),TODAY                                              
         B     VCLP1                                                            
                                                                                
VCTODAY  EQU   *                   TODAY=                                       
         CLC   P+6(3),=C'YES'                                                   
         BNE   VCERR1                                                           
         MVI   TDYONLY,C'Y'                                                     
         B     VCLP1                                                            
                                                                                
VCFILE   EQU   *                   FILE=                                        
         LA    RF,P+5                                                           
         BAS   RE,PACKIN                                                        
         BZ    VCERR1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8             CONVERT NUMBER TO BINARY                      
         B     *+10                                                             
         PACK  FILEMAX,P+5(0)                                                   
         B     VCLP1                                                            
                                                                                
VCTRANS  EQU   *                   TRANS=                                       
         LA    RF,P+6                                                           
         BAS   RE,PACKIN                                                        
         BZ    VCERR1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8             CONVERT NUMBER TO BINARY                      
         B     *+10                                                             
         PACK  TRANSMAX,P+6(0)                                                  
         B     VCLP1                                                            
                                                                                
VCSUPP   EQU   *                   SUPP=                                        
         LA    RF,P+5                                                           
         BAS   RE,PACKIN                                                        
         BZ    VCERR1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8             CONVERT NUMBER TO BINARY                      
         B     *+10                                                             
         PACK  SUPPMAX,P+5(0)                                                   
         B     VCLP1                                                            
                                                                                
VCOTHER  EQU   *                   OTHER=                                       
         LA    RF,P+6                                                           
         BAS   RE,PACKIN                                                        
         BZ    VCERR1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8             CONVERT NUMBER TO BINARY                      
         B     *+10                                                             
         PACK  OTHERMAX,P+6(0)                                                  
         B     VCLP1                                                            
                                                                                
VCBUCKET EQU   *                   BUCKET=                                      
         LA    RF,P+7                                                           
         BAS   RE,PACKIN                                                        
         BZ    VCERR1                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8             CONVERT NUMBER TO BINARY                      
         B     *+10                                                             
         PACK  BUCKMAX,P+7(0)                                                   
         B     VCLP1                                                            
*                                                                               
PACKIN   EQU   *                                                                
         SR    R1,R1               GET STRING LENGTH                            
         LA    R0,8                                                             
*                                                                               
PIN010   CLI   0(RF),C' '                                                       
         BE    PINOK                                                            
         CLI   0(RF),C'0'                                                       
         BL    PINNO                                                            
         CLI   0(RF),C'9'                                                       
         BH    PINNO                                                            
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PIN010                                                        
         B     PINNO                                                            
*                                                                               
PINNO    SR    R1,R1                                                            
PINOK    LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ RECOVERY FILE                                                  *         
***********************************************************************         
PROCREC  NTR1                                                                   
         L     R4,ASYSTAB                                                       
*                                                                               
PROC010  C     R4,ASYSTABL                                                      
         BE    PROC110                                                          
         GOTO1 GETSYS,DMCB,0(R4)                                                
         MVC   P(16),=C'PROCESS SYSTEM: '                                       
         MVC   P+18(7),0(R4)                                                    
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'FILE MAX:        '                                      
         EDIT  (P4,FILEMAX),(9,P+18),ZERO=NOBLANK,ALIGN=LEFT                    
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'TRANSACTION MAX: '                                      
         EDIT  (P4,TRANSMAX),(9,P+18),ZERO=NOBLANK,ALIGN=LEFT                   
         GOTO1 VPRINTER                                                         
         CLI   DMSYSNUM,X'06'                                                   
         BE    PROC011                                                          
         MVC   P(17),=C'SUPPLIER MAX:    '                                      
         EDIT  (P4,SUPPMAX),(9,P+18),ZERO=NOBLANK,ALIGN=LEFT                    
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'OTHER MAX:       '                                      
         EDIT  (P4,OTHERMAX),(9,P+18),ZERO=NOBLANK,ALIGN=LEFT                   
         GOTO1 VPRINTER                                                         
         CLI   AGYFILT,0                                                        
         BE    PROC012                                                          
         MVC   P(16),=C'AGENCY FILTER:  '                                       
         GOTO1 VHEXOUT,PARM,AGYFILT,P+18,1,=C'TOG'                              
         GOTO1 VPRINTER                                                         
         B     PROC012                                                          
*                                                                               
PROC011  MVC   P(17),=C'BUCKET MAX:      '                                      
         EDIT  (P4,BUCKMAX),(9,P+18),ZERO=NOBLANK,ALIGN=LEFT                    
         GOTO1 VPRINTER                                                         
         CLI   CPYFILT,0                                                        
         BE    PROC012                                                          
         MVC   P(16),=C'COMPANY FILTER: '                                       
         GOTO1 VHEXOUT,PARM,CPYFILT,P+18,1,=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                                                               
PROC012  CLI   MEDFILT,0                                                        
         BE    PROC014                                                          
         MVC   P(16),=C'MEDIA FILTER:   '                                       
         MVC   P+18(1),MEDFILT                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
PROC014  LA    R8,IOL                                                           
         USING RECDS,R8                                                         
         CLI   INTAPE,C'Y'                                                      
         BNE   PROC016                                                          
         OPEN  (RCVTAPE,(INPUT))                                                
         B     PROC020                                                          
*                                                                               
PROC016  GOTO1 VDATAMGR,DMCB,DMOPEN,DMSYS,DMFLIST,IOL                           
*                                                                               
PROC020  CLI   INTAPE,C'Y'                                                      
         BNE   PROC022                                                          
         LR    R0,R8                                                            
         GET   RCVTAPE,(0)                                                      
         B     PROC040                                                          
*                                                                               
PROC022  L     R0,=A(RECBUFF)                                                   
         GOTO1 VDATAMGR,DMCB,(X'FF',DMRSEQ),DMRFIL,DMDA,(R0),ATRKBUFF           
         TM    8(R1),X'80'         TEST E-O-F POSTED                            
         BO    PROC100                                                          
         CLI   8(R1),0             TEST ERROR POSTED                            
         BE    PROC030                                                          
*                                                                               
*                                  RECOVERY FILE ERRORS                         
*                                                                               
         MVC   P(28),=C'DISK ERROR ON RECOVERY FILE='                           
         MVC   P+28(8),DMRFIL                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'DA=XXXXXXXX,DMCB='                                      
         GOTO1 VHEXOUT,PARM,DMDA,P+34,,=C'TOG'                                  
         GOTO1 (RF),(R1),DMCB,P+17,20,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'RUN CONTINUING'                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         LH    R3,DMDA             BUMP TO NEXT TRACK                           
         LA    R3,1(R3)                                                         
         STH   R3,DMDA                                                          
         MVI   DMDA+2,0            SET RECNUM TO ZERO                           
         B     PROC020                                                          
*                                                                               
*                                  RECOVERY FILE RCORD FOUND OK                 
*                                                                               
PROC030  LH    R1,DMCB+18          GET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         XC    RECLN(4),RECLN                                                   
         STH   R1,RECLN                                                         
         LA    RF,RECVHDR                                                       
         SH    R1,=H'4'                                                         
         LR    RE,R0                                                            
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO TAPE BUFFER                   
*                                                                               
PROC040  TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    PROC020                                                          
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    PROC020                                                          
         BAS   RE,FILTREC                                                       
         BNE   PROC020                                                          
         BAS   RE,UTABLE                                                        
         BAS   RE,UTOTALS                                                       
         B     PROC020                                                          
*                                                                               
PROC100  BAS   RE,PTABLE                                                        
         LA    R4,10(R4)                                                        
         B     PROC010                                                          
*                                                                               
PROC110  BAS   RE,PTOTALS                                                       
         B     PROCOK                                                           
*                                                                               
PROCNO   B     NO                                                               
PROCOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* FILTER RECOVERY RECORDS                                             *         
***********************************************************************         
FILTREC  NTR1                                                                   
         CLI   TDYONLY,C'Y'                                                     
         BNE   *+14                                                             
         CLC   TODAYB,RDATE                                                     
         BNE   FILTNO                                                           
         CLI   DMSYSNUM,X'04'                                                   
         BNE   FILT100                                                          
         MVC   RECTYPE,RECVHDR+L'RECVHDR+1                                      
         ZIC   R1,RECVHDR+L'RECVHDR                                             
         SRL   R1,4                                                             
         STC   R1,AGENCY                                                        
         CLI   AGYFILT,0                                                        
         BE    FILT010                                                          
         CLI   AGENCY,0                                                         
         BE    FILTNO                                                           
         CLC   AGENCY,AGYFILT                                                   
         BNE   FILTNO                                                           
         B     FILTOK                                                           
*                                                                               
FILT010  ZIC   R1,RECVHDR+L'RECVHDR                                             
         STC   R1,MEDIA                                                         
         NI    MEDIA,X'0F'                                                      
         CLI   MEDFILT,0                                                        
         BE    FILTOK                                                           
         CLI   MEDIA,X'00'                                                      
         BNE   FILT020                                                          
         CLI   MEDFILT,C'T'                                                     
         BE    FILTOK                                                           
         B     FILTNO                                                           
*                                                                               
FILT020  CLI   MEDIA,X'01'                                                      
         BNE   FILTNO                                                           
         CLI   MEDFILT,C'P'                                                     
         BE    FILTOK                                                           
         B     FILTNO                                                           
*                                                                               
FILT100  CLI   DMSYSNUM,X'06'                                                   
         BNE   FILTOK                                                           
         LA    R4,RECVHDR+L'RECVHDR                                             
         GOTO1 VRECTYP,DMCB,(C'D',(R4))                                         
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         CLI   CPYFILT,0                                                        
         BE    FILTOK                                                           
         CLC   CPYFILT,COMPANY                                                  
         BNE   FILTNO                                                           
         B     FILTOK                                                           
*                                                                               
FILTNO   B     NO                                                               
FILTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE RECOVERY STATISTICS FILE TABLE                               *         
***********************************************************************         
UTABLE   NTR1                                                                   
         CLI   DMSYSNUM,X'04'                                                   
         BE    UTABMED                                                          
         CLI   DMSYSNUM,X'06'                                                   
         BE    UTABACC                                                          
         B     UTABOK                                                           
*                                                                               
UTABMED  CLI   RFILTY,MFILEQU                                                   
         BNE   UTABOK                                                           
         LA    R2,MFILTAB                                                       
         LA    R3,MTYPTAB                                                       
         B     UTABALL                                                          
*                                                                               
UTABACC  CLI   RFILTY,AFILEQU                                                   
         BNE   UTABOK                                                           
         LA    R2,AFILTAB                                                       
         LA    R3,ATYPTAB                                                       
         B     UTABALL                                                          
         USING SFILTABD,R2                                                      
*                                                                               
UTABALL  EQU   *                                                                
*                                                                               
UTAB010  CLI   0(R3),X'FF'                                                      
         BE    UTAB030                                                          
         CLC   RECTYPE,0(R3)                                                    
         BE    UTAB030                                                          
         LA    R3,2(R3)                                                         
         B     UTAB010                                                          
*                                                                               
UTAB030  CLI   SFILGRP,X'FF'       TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   SFILGRP,1(R3)       TEST FILE GROUP                              
         BE    UTAB040                                                          
         LA    R2,SFILTABL(R2)                                                  
         B     UTAB030                                                          
*                                                                               
UTAB040  CLI   RRECTY,X'01'                                                     
         BE    UTAB050                                                          
         CLI   RRECTY,X'02'                                                     
         BE    UTAB050                                                          
         CLI   RRECTY,X'03'                                                     
         BE    UTAB050                                                          
         DC    H'00'                                                            
UTAB050  ZIC   R1,RRECTY           UPDATE FILE ACCUMS                           
         SLL   R1,2                                                             
         LA    R1,SFILCNTS-L'SFILCNTS(R1)                                       
         AP    0(4,R1),=PL4'1'                                                  
         CLI   RRECTY,X'01'                                                     
         BE    UTAB060                                                          
         AP    SFILTOTS,=PL4'1'                                                 
*                                                                               
UTAB060  BAS   RE,UTIMTAB                                                       
         B     UTABOK                                                           
*                                                                               
UTABNO   B     NO                                                               
UTABOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE TIME TABLES                                                  *         
***********************************************************************         
UTIMTAB  NTR1                                                                   
         L     R4,ATIMTAB                                                       
*                                                                               
UTIM010  C     R4,ATIMTABL                                                      
         BE    UTIMOK                                                           
         LA    R2,4(R4)                                                         
         USING SFILTABD,R2                                                      
         LA    R3,MTYPTAB                                                       
         CLI   DMSYSNUM,X'06'                                                   
         BNE   *+8                                                              
         LA    R3,ATYPTAB                                                       
*                                                                               
UTIM020  CLI   0(R3),X'FF'                                                      
         BE    UTIM030                                                          
         CLC   RECTYPE,0(R3)                                                    
         BE    UTIM030                                                          
         LA    R3,2(R3)                                                         
         B     UTIM020                                                          
*                                                                               
UTIM030  CLI   SFILGRP,X'FF'       TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   SFILGRP,1(R3)       TEST FILE GROUP                              
         BE    UTIM040                                                          
         LA    R2,SFILTABL(R2)                                                  
         B     UTIM030                                                          
*                                                                               
UTIM040  ICM   R0,15,RTIME                                                      
         BM    *+8                                                              
         SLL   R0,4                                                             
         SRL   R0,16                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(R4)                                                       
         CR    R0,RF                                                            
         BL    UTIM060                                                          
         ICM   RF,3,2(R4)                                                       
         CR    R0,RF                                                            
         BH    UTIM060                                                          
         CLI   RRECTY,X'01'                                                     
         BE    UTIM050                                                          
         CLI   RRECTY,X'02'                                                     
         BE    UTIM050                                                          
         CLI   RRECTY,X'03'                                                     
         BE    UTIM050                                                          
         DC    H'00'                                                            
UTIM050  ZIC   R1,RRECTY           UPDATE FILE ACCUMS                           
         SLL   R1,2                                                             
         LA    R1,SFILCNTS-L'SFILCNTS(R1)                                       
         AP    0(4,R1),=PL4'1'                                                  
         CLI   RRECTY,X'01'                                                     
         BE    UTIM060                                                          
         AP    SFILTOTS,=PL4'1'                                                 
*                                                                               
UTIM060  CLI   DMSYSNUM,X'04'                                                   
         BE    *+12                                                             
         LA    R4,AFILTABX-AFILTAB+5(R4)                                        
         B     UTIM010                                                          
         LA    R4,MFILTABX-MFILTAB+5(R4)                                        
         B     UTIM010                                                          
*                                                                               
UTIMNO   B     NO                                                               
UTIMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE RECOVERY STATISTICS TOTALS                                   *         
***********************************************************************         
UTOTALS  NTR1                                                                   
         LA    R2,FILETAB                                                       
         USING FILETABD,R2                                                      
*                                                                               
UTOT010  CLI   FILENUM,0           TEST E-O-T                                   
         BE    *+10                                                             
         CLC   FILENUM,RFILTY                                                   
         BE    *+12                                                             
         LA    R2,FILETABL(R2)                                                  
         B     UTOT010                                                          
         ZIC   R1,RRECTY           UPDATE FILE ACCUMS                           
         SLL   R1,2                                                             
         LA    R1,FILECNTS-L'FILECNTS(R1)                                       
         AP    0(4,R1),=PL4'1'                                                  
         CLI   RRECTY,X'01'                                                     
         BE    UTOTOK                                                           
         AP    FILETOTS,=PL4'1'                                                 
         B     UTOTOK                                                           
*                                                                               
UTOTNO   B     NO                                                               
UTOTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT RECOVERY FILE STATISTICS TABLES                               *         
***********************************************************************         
PTABLE   NTR1                                                                   
         MVI   ACTIVITY,C'N'                                                    
         MVC   TITLE,SPACES                                                     
         MVC   MID1(80),SFMID1L                                                 
         MVC   MID2(80),SFMID2L                                                 
         ZAP   LINE,=P'99'                                                      
         CLI   DMSYSNUM,X'04'                                                   
         BE    PTABMED                                                          
         CLI   DMSYSNUM,X'06'                                                   
         BE    PTABACC                                                          
         B     PTABOK                                                           
*                                                                               
PTABMED  EQU   *                                                                
         MVC   TITLE(18),=C'MEDIA SYSTEM TABLE'                                 
         LA    R2,MFILTAB                                                       
         B     PTABALL                                                          
*                                                                               
PTABACC  EQU   *                                                                
         MVC   TITLE(20),=C'ACCOUNT SYSTEM TABLE'                               
         LA    R2,AFILTAB                                                       
         B     PTABALL                                                          
*                                                                               
PTABALL  EQU   *                                                                
*                                                                               
         USING SFILTABD,R2                                                      
PTAB010  CLI   SFILGRP,X'FF'                                                    
         BE    PTAB040                                                          
         MVI   ACTIVITY,C'Y'                                                    
         MVC   P(13),SFILGNAM                                                   
         LA    R1,P+14                                                          
         LA    R3,SFILCNTS                                                      
         LA    RF,SFILCNTN                                                      
PTAB020  EDIT  (P4,0(R3)),(9,0(R1)),ZERO=NOBLANK                                
         LA    R1,10(R1)                                                        
         LA    R3,4(R3)                                                         
         BCT   RF,PTAB020                                                       
         LR    RF,R1                                                            
         GOTO1 PRINTPER,DMCB,(RF),(R2)                                          
         GOTO1 VPRINTER                                                         
PTAB030  LA    R2,SFILTABL(R2)                                                  
         B     PTAB010                                                          
*                                                                               
PTAB040  CLI   ACTIVITY,C'Y'                                                    
         BE    PTAB050                                                          
         MVC   P(17),=C'NO ACTIVITY TODAY'                                      
         GOTO1 VPRINTER                                                         
         B     PTAB050                                                          
*                                                                               
PTAB050  BAS   RE,PTIMTAB                                                       
         B     PTABOK                                                           
*                                                                               
PTABNO   B     NO                                                               
PTABOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT TIME TABLES                                                   *         
***********************************************************************         
PTIMTAB  NTR1                                                                   
         MVI   ACTIVITY,C'N'                                                    
         L     R4,ATIMTAB                                                       
*                                                                               
PTIM002  C     R4,ATIMTABL                                                      
         BE    PTIMOK                                                           
         MVC   TITLE,SPACES                                                     
         MVC   MID1(80),SFMID1L                                                 
         MVC   MID2(80),SFMID2L                                                 
         CLI   DMSYSNUM,X'04'                                                   
         BE    PTIMMED                                                          
         CLI   DMSYSNUM,X'06'                                                   
         BE    PTIMACC                                                          
         B     PTIMOK                                                           
*                                                                               
PTIMMED  EQU   *                                                                
         MVC   TITLE(23),=C'MEDIA SYSTEM TIME TABLE'                            
         B     PTIMALL                                                          
*                                                                               
PTIMACC  EQU   *                                                                
         MVC   TITLE(25),=C'ACCOUNT SYSTEM TIME TABLE'                          
         B     PTIMALL                                                          
*                                                                               
PTIMALL  EQU   *                                                                
         LA    RF,9                                                             
         GOTO1 VTIMBER,PARM,(X'00',(RF)),(X'01',0(R4)),(X'01',MID1+6)           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         L     RF,ATIMTABL                                                      
         MVC   0(4,RF),FULL                                                     
         ZAP   LINE,=P'99'                                                      
*                                                                               
         LA    R2,4(R4)                                                         
         USING SFILTABD,R2                                                      
PTIM010  CLI   SFILGRP,X'FF'                                                    
         BE    PTIM040                                                          
         MVI   ACTIVITY,C'Y'                                                    
         MVC   P(13),SFILGNAM                                                   
         LA    R1,P+14                                                          
         LA    R3,SFILCNTS                                                      
         LA    RF,SFILCNTN                                                      
PTIM020  EDIT  (P4,0(R3)),(9,0(R1)),ZERO=NOBLANK                                
         LA    R1,10(R1)                                                        
         LA    R3,4(R3)                                                         
         BCT   RF,PTIM020                                                       
         LR    RF,R1                                                            
         GOTO1 PRINTPER,DMCB,(RF),(R2)                                          
         GOTO1 VPRINTER                                                         
PTIM030  LA    R2,SFILTABL(R2)                                                  
         B     PTIM010                                                          
*                                                                               
PTIM040  CLI   ACTIVITY,C'Y'                                                    
         BE    PTIM050                                                          
         MVC   P(17),=C'NO ACTIVITY TODAY'                                      
         GOTO1 VPRINTER                                                         
         B     PTIM050                                                          
*                                                                               
PTIM050  CLI   DMSYSNUM,X'04'                                                   
         BE    *+12                                                             
         LA    R4,AFILTABX-AFILTAB+5(R4)                                        
         B     PTIM002                                                          
         LA    R4,MFILTABX-MFILTAB+5(R4)                                        
         B     PTIM002                                                          
*                                                                               
PTIMNO   B     NO                                                               
PTIMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT RECOVERY STATISTICS TOTALS                                    *         
***********************************************************************         
PTOTALS  NTR1                                                                   
         MVI   ACTIVITY,C'N'                                                    
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(20),=C'RECOVERY FILE TOTALS'                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1(L'FTMID1L),FTMID1L                                          
         MVC   MID2(L'FTMID2L),FTMID2L                                          
         ZAP   LINE,=P'99'                                                      
*                                                                               
         LA    R2,FILETAB                                                       
         USING FILETABD,R2                                                      
PTOT010  CLI   FILENUM,X'FF'                                                    
         BE    PTOT040                                                          
         MVI   ACTIVITY,C'Y'                                                    
         CP    FILETOTS,=PL4'0'                                                 
         BE    PTOT030                                                          
         MVC   P(7),FILENAME                                                    
         LA    R1,P+10                                                          
         LA    R3,FILECNTS                                                      
         LA    RF,FILECNTN                                                      
PTOT020  EDIT  (P4,0(R3)),(9,0(R1)),ZERO=NOBLANK                                
         LA    R1,10(R1)                                                        
         LA    R3,4(R3)                                                         
         BCT   RF,PTOT020                                                       
         GOTO1 VPRINTER                                                         
PTOT030  LA    R2,FILETABL(R2)                                                  
         B     PTOT010                                                          
*                                                                               
PTOT040  CLI   ACTIVITY,C'Y'                                                    
         BE    PTOTOK                                                           
         MVC   P(17),=C'NO ACTIVITY TODAY'                                      
         GOTO1 VPRINTER                                                         
         B     PTOTOK                                                           
*                                                                               
PTOTNO   B     NO                                                               
PTOTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD SYSTEM INFO TABLE                                             *         
***********************************************************************         
BLDSYS   NTR1                                                                   
         LR    R2,R1               R2=A(PARM LIST)                              
         L     RE,0(R2)            RE=A(SUB SYSTEM NUMBER)                      
         MVC   SNUM,0(RE)                                                       
*                                                                               
         LA    RE,SNUMLIST         LOOK-UP LETTER IN LIST                       
BSYS010  CLI   0(RE),0             TEST E-O-L                                   
         BE    BSYSERR1                                                         
         CLC   SNUM,0(RE)          MATCH INPUT TO TABLE                         
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     BSYS010                                                          
         LA    RF,SNUMLIST                                                      
         SR    RE,RF               RE=SYSTEM FILE SET NUMBER                    
         STC   RE,SNUMB                                                         
*                                                                               
* READ & PROCESS SYSTEM LIST RECORD                                             
*                                                                               
         LA    R3,IOL                                                           
         USING CTWREC,R3           R3=A(RECORD)                                 
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ RECORD                      
*                                                                               
         LA    R3,CTWDATA          FIND SYSTEM ELEMENT ON RECORD                
         USING SYSELD,R3           R3=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
BSYS020  CLI   SYSEL,0             TEST E-O-R                                   
         BE    BSYSERR3                                                         
         CLI   SYSEL,SYSELQ                                                     
         BNE   BSYS030                                                          
         CLC   SYSSYS,SYSTEM       MATCH SYSTEM NUMBER                          
         BNE   BSYS030                                                          
         CLC   SYSNUM,SNUMB        MATCH SYSTEM RELATIVE NUMBER                 
         BE    BSYS040                                                          
BSYS030  IC    R0,SYSLEN                                                        
         AR    R3,R0                                                            
         B     BSYS020                                                          
*                                                                               
BSYS040  LA    R1,SYSNAME          RETURN A(DATA) IN P1                         
         ST    R1,0(R2)                                                         
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ERRORS & EXIT                                                                 
*                                                                               
BSYSERR1 MVI   1(R2),2             INVALID FILE SET                             
         B     NO                                                               
BSYSERR3 MVI   1(R2),3             INVALID SYSTEM NUMBER                        
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM INFO FROM TABLES                                         *         
***********************************************************************         
GETSYS   NTR1                                                                   
         LR    R2,R1               R2=A(PARM LIST)                              
         L     R3,0(R2)            RE=A(SUB SYSTEM NUMBER)                      
         USING SYSNAME,R3                                                       
         LA    RE,SNUMLIST         LOOK-UP LETTER IN LIST                       
         SR    R0,R0                                                            
         IC    R0,SYSNUM                                                        
         LTR   R0,R0                                                            
         BZ    GET020                                                           
*                                                                               
GET010   CLI   0(RE),0             TEST E-O-L                                   
         BNE   *+6                                                              
         DC    H'00'                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,GET010                                                        
*                                                                               
GET020   MVC   SNUM,0(RE)                                                       
         LA    RE,DMFTAB                                                        
*                                                                               
GET040   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   0(1,RE),SYSSYS                                                   
         BE    GET044                                                           
*                                                                               
GET042   LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   GET042                                                           
         LA    RE,1(RE)                                                         
         B     GET040                                                           
GET044   MVC   DMSYSNUM,0(RE)                                                   
         MVC   DMSYS,1(RE)                                                      
         MVC   DMFLIST,SPACES                                                   
         LA    RF,DMFLIST                                                       
         LA    RE,9(RE)                                                         
GET046   CLI   0(RE),0                                                          
         BE    GET050                                                           
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     GET046                                                           
*                                                                               
*                                  SET UTL SENUM & PATCH DTFS                   
GET050   L     RE,=A(UTL)                                                       
         MVC   4(1,RE),SYSSEN                                                   
         ICM   RE,15,=V(MEDGEN)    PATCH MEDGEN DTF IF RESOLVED                 
         BZ    *+10                                                             
         MVC   46(1,RE),SNUM                                                    
         CLI   TESTSYS,C'Y'        IS THIS THE TEST SYSTEM                      
         BNE   GSYSOK                                                           
         GOTO1 =V(DATAMGR),DMCB,=C'DTFADD',=C'MEDDIR'                           
         L     RE,12(R1)           YES - PATCH DTF NAMES                        
         MVI   28(RE),C'0'                                                      
         GOTO1 (RF),(R1),=C'DTFADD',=C'FIL'                                     
         L     RE,12(R1)                                                        
         MVI   28(RE),C'0'                                                      
         GOTO1 (RF),(R1),=C'DTFADD',=C'MEDREQ'                                  
         L     RE,12(R1)                                                        
         MVI   28(RE),C'0'                                                      
         GOTO1 (RF),(R1),=C'DTFADD',=C'MEDRCV'                                  
         L     RE,12(R1)                                                        
         MVI   28(RE),C'0'                                                      
         GOTO1 (RF),(R1),=C'UPDID'                                              
         L     RE,12(R1)                                                        
         MVC   0(2,RE),=C'XX'                                                   
         ICM   RE,15,=V(MEDGEN)    TEST MEDGEN IS MEDGEN1                       
         BZ    GSYSOK                                                           
         MVI   46(RE),C'1'                                                      
         B     GSYSOK                                                           
GSYSOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PERCENT FIGURE                                                *         
***********************************************************************         
PRINTPER NTR1                                                                   
         L     R2,4(R1)                                                         
         USING SFILTABD,R2                                                      
         L     R3,0(R1)                                                         
         XC    DUB,DUB                                                          
         MVC   DUB+4(4),SFILTOTS                                                
         CLI   SFILGRP,X'01'                                                    
         BNE   *+14                                                             
         MVC   FULL,FILEMAX                                                     
         B     PPER010                                                          
         CLI   SFILGRP,X'02'                                                    
         BNE   *+14                                                             
         MVC   FULL,TRANSMAX                                                    
         B     PPER010                                                          
         CLI   SFILGRP,X'03'                                                    
         BNE   *+14                                                             
         MVC   FULL,SUPPMAX                                                     
         B     PPER010                                                          
         CLI   SFILGRP,X'04'                                                    
         BNE   *+14                                                             
         MVC   FULL,OTHERMAX                                                    
         B     PPER010                                                          
         CLI   SFILGRP,X'05'                                                    
         BNE   *+14                                                             
         MVC   FULL,BUCKMAX                                                     
         B     PPER010                                                          
*                                                                               
PPER010  LA    RF,FULL                                                          
         CP    FULL,=PL4'0'                                                     
         BE    PPER020                                                          
         MP    DUB(8),=PL4'10000'                                               
         DP    DUB(8),FULL(4)                                                   
         LA    RF,DUB                                                           
*                                                                               
PPER020  EDIT  (P4,0(RF)),(9,0(R3)),ZERO=NOBLANK,2                              
         CLI   5(R3),C' '                                                       
         BNE   *+8                                                              
         MVI   5(R3),C'0'                                                       
*                                                                               
PPERX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
ERRPRT   NTR1                                                                   
         LA    RE,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRMSG0),0(RE)                                            
         GOTO1 VPRINTER                                                         
         MVI   ERROR,0                                                          
         XIT1                                                                   
                                                                                
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
SFMID1L  DC    C'TYPE  ALL TIMES  COPIES   CHANGES      ADDS'                   
         DC    C'    TOTALS   PERCENT                       '                   
SFMID2L  DC    C'----  ---------  ------   -------      ----'                   
         DC    C'    ------   -------                       '                   
*                                                                               
                                                                                
MFILEQU  EQU   X'42'                                                            
MFILTAB  DS    0XL30                                                            
         DC    X'01',CL13'FILE        ',4PL4'0'                                 
         DC    X'02',CL13'TRANSACTION ',4PL4'0'                                 
         DC    X'03',CL13'SUPPLIER    ',4PL4'0'                                 
         DC    X'04',CL13'OTHERS      ',4PL4'0'                                 
MFILTABX DC    X'FF'                                                            
                                                                                
MTYPTAB  DS    0XL2                                                             
         DC    X'C1',X'01'                                                      
         DC    X'C2',X'01'                                                      
         DC    X'F4',X'01'                                                      
         DC    X'C3',X'01'                                                      
         DC    X'C5',X'01'                                                      
         DC    X'C7',X'01'                                                      
         DC    X'C8',X'01'                                                      
         DC    X'C9',X'01'                                                      
         DC    X'E2',X'02'                                                      
         DC    X'D2',X'03'                                                      
         DC    X'E7',X'03'                                                      
MTYPTABX DC    X'FF',X'04'                                                      
*                                                                               
                                                                                
AFILEQU  EQU   X'6A'                                                            
AFILTAB  DS    0XL30                                                            
         DC    X'01',CL13'FILE        ',4PL4'0'                                 
         DC    X'02',CL13'TRANSACTION ',4PL4'0'                                 
         DC    X'05',CL13'BUCKET      ',4PL4'0'                                 
AFILTABX DC    X'FF'                                                            
                                                                                
ATYPTAB  DS    0XL2                                                             
         DC    AL1(09,05)                                                       
         DC    AL1(10,02)                                                       
         DC    AL1(11,02)                                                       
ATYPTABX DC    X'FF',X'01'                                                      
         EJECT                                                                  
                                                                                
FTMID1L  DC    C'FILE         COPIES   CHANGES      ADDS    TOTALS'             
FTMID2L  DC    C'----         ------   -------      ----    ------'             
*                                                                               
                                                                                
FILETAB  DS    0XL24                                                            
         DC    X'41',C'MEDDIR ',4PL4'0'                                         
         DC    X'42',C'MEDFIL ',4PL4'0'                                         
         DC    X'43',C'MEDREQ ',4PL4'0'                                         
         DC    X'45',C'MEDWRK ',4PL4'0'                                         
         DC    X'51',C'MPLDIR ',4PL4'0'                                         
         DC    X'52',C'MPLFIL ',4PL4'0'                                         
         DC    X'53',C'MPLREQ ',4PL4'0'                                         
         DC    X'56',C'MPQDRA ',4PL4'0'                                         
         DC    X'57',C'MPQFLA ',4PL4'0'                                         
         DC    X'58',C'MPRDRA ',4PL4'0'                                         
         DC    X'59',C'MPRFLA ',4PL4'0'                                         
         DC    X'5A',C'BUDDIR ',4PL4'0'                                         
         DC    X'5B',C'BUDFIL ',4PL4'0'                                         
         DC    X'5C',C'MPRDRB ',4PL4'0'                                         
         DC    X'5D',C'MPRFLB ',4PL4'0'                                         
         DC    X'69',C'ACCDIR ',4PL4'0'                                         
         DC    X'61',C'ACCFIL ',4PL4'0'                                         
         DC    X'6A',C'ACCMST ',4PL4'0'                                         
         DC    X'6B',C'ACCARC ',4PL4'0'                                         
         DC    X'63',C'ACCREQ ',4PL4'0'                                         
         DC    X'66',C'ACCDAY ',4PL4'0'                                         
         DC    X'67',C'ACCWRK ',4PL4'0'                                         
         DC    X'A1',C'CTFILE ',4PL4'0'                                         
         DC    X'A3',C'CTREQ  ',4PL4'0'                                         
*&&UK*&& DC    X'A8',C'DEMOLD ',4PL4'0'                                         
*&&UK*&& DC    X'A9',C'DEMNEW ',4PL4'0'                                         
*&&UK*&& DC    X'AA',C'DEMDIR ',4PL4'0'                                         
*&&US*&& DC    X'AD',C'CTUSER ',4PL4'0'                                         
         DC    X'AE',C'GENDIR ',4PL4'0'                                         
         DC    X'AF',C'GENFIL ',4PL4'0'                                         
         DC    X'00',C'UNKNOWN',4PL4'0'                                         
FILETABX DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
SNUMLIST DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                    
                                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'SYSTEM='                                   
         DC    AL1(07,02),X'00',CL11'AGENCY='                                   
         DC    AL1(06,03),X'00',CL11'MEDIA='                                    
         DC    AL1(05,04),X'00',CL11'TIME='                                     
         DC    AL1(06,05),X'00',CL11'INPUT='                                    
         DC    AL1(05,06),X'00',CL11'DATE='                                     
         DC    AL1(06,07),X'00',CL11'TODAY='                                    
         DC    AL1(05,08),X'00',CL11'FILE='                                     
         DC    AL1(06,09),X'00',CL11'TRANS='                                    
         DC    AL1(05,10),X'00',CL11'SUPP='                                     
         DC    AL1(06,11),X'00',CL11'OTHER='                                    
         DC    AL1(08,12),X'00',CL11'COMPANY='                                  
         DC    AL1(07,13),X'00',CL11'BUCKET='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
                                                                                
* DMFILE TABLE                                                                  
*                                                                               
DMFTAB   DC    X'04',CL8'MEDIA  ',C'NMEDRCV X',X'00'                            
         DC    X'05',CL8'MPL    ',C'NMPLRCV X',X'00'                            
         DC    X'06',CL8'ACCOUNT',C'NACCRCV X',X'00'                            
         DC    X'0A',CL8'CONTROL',C'NCTRCVR X',X'00'                            
DMFTABX  DC    X'00'                                                            
UTYPENUM EQU   7                                                                
                                                                                
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
                                                                                
         EJECT                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
                                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VRECTYP  DC    V(ACRECTYP)                                                      
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
DMDA     DC    F'0'                                                             
COUNT    DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'MISSING SYSTEM DEFINITION'                                  
ERRMSG3  DC    CL40'INVALID SYSTEM NAME      '                                  
ERRMSG4  DC    CL40'ERROR 4                  '                                  
ERRMSG5  DC    CL40'ERROR 5                  '                                  
                                                                                
SORTCARD DC    C'SORT FIELDS=(5,1,A,29,25,A,23,3,A,9,4,A),FORMAT=BI,WOR*        
               K=1 '                                                            
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=0,MACRF=(GM),EODAD=PROC100                               
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'0A',XL3'00',XL56'00'                                      
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
                                                                                
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
* DSECT TO COVER MEDIA FILE TABLE                                               
*                                                                               
SFILTABD  DSECT                                                                 
SFILGRP  DS    XL1                 MEDFIL RECORD TYPE GROUP                     
SFILGNAM DS    CL13                MEDFIL GROUP NAME                            
SFILCNTS DS    0PL4                                                             
SFILCPYS DS    PL4                 N'COPIES                                     
SFILCHAS DS    PL4                 N'CHANGES                                    
SFILADDS DS    PL4                 N'ADDS                                       
SFILTOTS DS    PL4                 TOTAL N'RECORDS                              
SFILCNTN EQU   (*-SFILCNTS)/L'SFILCNTS                                          
SFILCNTL EQU   *-SFILCNTS                                                       
SFILTABL EQU   *-SFILTABD                                                       
                                                                                
* DSECT TO COVER FILE TABLE                                                     
*                                                                               
FILETABD DSECT                                                                  
FILENUM  DS    XL1                 FILE NUMBER                                  
FILENAME DS    CL7                 FILE NAME                                    
FILECNTS DS    0PL4                                                             
FILECPYS DS    PL4                 N'COPIES                                     
FILECHAS DS    PL4                 N'CHANGES                                    
FILEADDS DS    PL4                 N'ADDS                                       
FILETOTS DS    PL4                 TOTAL N'RECORDS                              
FILECNTN EQU   (*-FILECNTS)/L'FILECNTS                                          
FILECNTL EQU   *-FILECNTS                                                       
FILETABL EQU   *-FILETABD                                                       
                                                                                
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
                                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
EXPARMS  DS    50F                                                              
FULL     DS    F                                                                
ASYSLST  DS    A                                                                
COUNT1   DS    F                                                                
COUNT2   DS    F                                                                
BYTE     DS    XL1                                                              
FIRSTFLG DS    XL1                                                              
TODAY    DS    CL6                                                              
TODAYB   DS    XL3                                                              
RETCODE  DS    XL1                                                              
PASSFLAG DS    XL1                                                              
CONTROLF DS    XL1                                                              
AGYIND   DS    XL1                                                              
SYSTEM   DS    XL1                                                              
SNUM     DS    CL1                                                              
SNUMB    DS    XL1                                                              
TESTSYS  DS    CL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
*                                  SYSTEM TABLE POINTERS                        
ASYSTAB  DS    A                                                                
ASYSTABL DS    A                                                                
ASYSTABX DS    A                                                                
*                                                                               
ATRKBUFF DS    A                                                                
ARECBUFF DS    A                                                                
*                                                                               
ATIMTAB  DS    A                                                                
ATIMTABL DS    A                                                                
ATIMTABX DS    A                                                                
*                                                                               
AGYFILT  DS    XL1                                                              
CPYFILT  DS    XL1                                                              
MEDFILT  DS    XL1                                                              
RECTYPE  DS    XL1                                                              
AGENCY   DS    XL1                                                              
MEDIA    DS    XL1                                                              
COMPANY  DS    XL1                                                              
INTAPE   DS    CL1                                                              
TDYONLY  DS    CL1                                                              
FILEMAX  DS    PL4                                                              
TRANSMAX DS    PL4                                                              
SUPPMAX  DS    PL4                                                              
OTHERMAX DS    PL4                                                              
BUCKMAX  DS    PL4                                                              
DMSYSNUM DS    XL1                                                              
DMSYS    DS    CL8                                                              
DMFLIST  DS    CL80                                                             
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CTIKEY)                                                     
IOL      DS    F                                                                
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
                                                                                
RECBUFF  CSECT                                                                  
         DS    2048X                                                            
RECBUFFX EQU   *                                                                
                                                                                
SYSTAB   CSECT                                                                  
         DS    (10000)XL3                                                       
SYSTABX  EQU   *                                                                
                                                                                
TRKBUFF  CSECT                                                                  
         DS    (50000)XL1                                                       
TRKBUFFX EQU   *                                                                
                                                                                
TIMTAB   CSECT                                                                  
         DS    (20)XL(5+(MFILTABX-MFILTAB))                                     
TIMTABX  EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDREVIEW  09/21/11'                                      
         END                                                                    
