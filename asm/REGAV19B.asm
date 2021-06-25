*          DATA SET REGAV19B   AT LEVEL 010 AS OF 05/01/02                      
*PHASE T81319B,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
*INCLUDE RECUP                                                                  
         TITLE 'T81319 - REPPAK FILE MAINT - RATE CARD AVAIL COPY'              
********************************************************************            
*                                                                               
*        DEC98 - RDETAIL COPY ('Z' INV RATE RECORDS)                            
*                                                                               
********************************************************************            
T81319   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81319,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81319+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         L     RF,=V(RECUP)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VRECUP                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*                                                                               
         LR    R3,RA               MOVE PROFILE TO LOCAL STORAGE                
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
*                                                                               
MAIN20   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       REPORT                                       
         BE    PREP                                                             
         CLI   MODE,VALREC         VALIDATE RECORDS                             
         BE    VREC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*        VALIDATE KEY                                                           
*****************************************************************               
VKEY     DS    0H                                                               
         XC    PROCFLAG,PROCFLAG                                                
         OI    PROCFLAG,FRATE      PROCESSING FROM DETAILS                      
*                                                                               
         MVC   REPHLD,AGENCY       SAVE THE REP                                 
*                                                                               
         GOTO1 =A(VALSTA),DMCB,RNCSSTAH,RR=RELO                                 
*                                                                               
*   VALIDATE FROM DETAILS                                                       
*                                                                               
         LA    R2,RNCFCODH         VALIDATE FROM RATE CODE                      
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 CHARS                     
         JL    ERREND                                                           
         MVC   FCODHLD,8(R2)                                                    
         OC    FCODHLD,SPACES                                                   
         MVC   CODHLD,FCODHLD                                                   
*                                                                               
         GOTO1 =A(VALLEN),DMCB,RNCFLENH,RR=RELO                                 
         MVC   FLENHLD,LENHLD                                                   
*                                                                               
         GOTO1 =A(GETQTYR),DMCB,RNCFQTRH,RR=RELO                                
         MVC   FQTRHLD,QTRHLD                                                   
         MVC   FQTRBIT,QTRBIT                                                   
         MVC   FYEARHLD,YEARHLD                                                 
*                                                                               
         GOTO1 =A(CHKRTCD),DMCB,RNCFCODH,RR=RELO     VALID RATE CODE?           
*                                                                               
         XC    DPTHLD,DPTHLD                                                    
         LA    R2,RNCFDPTH         FROM DAYPART FILTER                          
         CLI   5(R2),0             ANY DAYPART FILTER                           
         BE    VK40                NO                                           
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BE    VK40                                                             
*                                                                               
         CLI   5(R2),1                                                          
         JNE   ERREND                                                           
         MVC   DPTHLD,8(R2)                                                     
*                                                                               
VK40     DS    0H                                                               
         NI    PROCFLAG,X'FF'-FRATE                                             
*                                                                               
*   VALIDATE TO DETAILS                                                         
*                                                                               
         XC    OPTFLAG,OPTFLAG                                                  
         OI    PROCFLAG,TRATE      PROCESSING TO DETAILS                        
*                                                                               
         LA    R2,RNCTCODH         VALIDATE TO RATE CODE                        
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   TCODHLD,FCODHLD     SAME CODE AS FROM                            
         B     VK50                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 CHARS                     
         JL    ERREND                                                           
         MVC   TCODHLD,8(R2)                                                    
         OC    TCODHLD,SPACES                                                   
         MVC   CODHLD,TCODHLD                                                   
         OI    OPTFLAG,TCODE                                                    
*                                                                               
VK50     DS    0H                                                               
         LA    R2,RNCTLENH         VALIDATE TO LENGTH                           
         CLI   5(R2),0             ANY TO LENGTH?                               
         BNE   *+14                                                             
         MVC   TLENHLD,FLENHLD     SAME LENGTH AS FROM                          
         B     VK60                                                             
*                                                                               
         GOTO1 =A(VALLEN),DMCB,RNCTLENH,RR=RELO                                 
         MVC   TLENHLD,LENHLD                                                   
         OI    OPTFLAG,TLEN                                                     
*                                                                               
VK60     DS    0H                                                               
         XC    TQTRYR,TQTRYR                                                    
*                                                                               
         LA    R2,RNCTQTRH                                                      
         CLI   5(R2),0             ANY TO QTR?                                  
         BNE   VK65                                                             
         MVC   TQTRYR(1),FQTRHLD     SAME AS FROM                               
         MVC   TQTRYR+1(1),FQTRBIT                                              
         MVC   TQTRYR+2(1),FYEARHLD                                             
         MVI   TQTRYR+3,X'FF'      DENOTES END                                  
         B     VK70                                                             
*                                                                               
VK65     DS    0H                  GET TO QTR/YR DETAILS                        
         GOTO1 =A(GETTQTYR),DMCB,RNCTQTRH,RR=RELO                               
         OI    OPTFLAG,TQTR                                                     
*                                                                               
VK70     DS    0H                                                               
         GOTO1 =A(CHKRTCD),DMCB,RNCTCODH,RR=RELO     VALID RATE CODE?           
*                                                                               
         LA    R2,RNCRADJH                                                      
         CLI   5(R2),0                                                          
         BE    VK75                                                             
*                                                                               
         OI    OPTFLAG,TRTADJ                                                   
         GOTO1 =A(GETADJ),RNCRADJH,RR=RELO       GET RATE ADJUSTMENTS           
*                                                                               
VK75     DS    0H                                                               
         LA    R2,RNCSUPH                                                       
         CLI   5(R2),0                                                          
         BE    VK80                                                             
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    VK80                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
         OI    PROCFLAG,SUPCOPY                                                 
*                                                                               
VK80     DS    0H                                                               
         CLI   OPTFLAG,0                                                        
         BE    NOTOFLD                                                          
*                                                                               
         NI    PROCFLAG,X'FF'-TRATE                                             
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    VKEYX                                                            
*                                                                               
         MVC   KEY(27),KEYSAVE     SET GENCON ERROR                             
*                                                                               
VKEYX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RECORD                                                        
*****************************************************************               
VREC     DS    0H                                                               
*                                                                               
         DC    H'00'                                                            
*                                                                               
VRECX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        PRINT REPORT                                                           
*****************************************************************               
PREP     DS    0H                                                               
*                                                                               
         GOTO1 =A(PRNTRPT),RR=RELO                                              
*                                                                               
PREPX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*****************************************************************               
*                                                                               
ERREND   GOTO1 ERREX                                                            
*                                                                               
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        PRINT REPORT                                                           
*****************************************************************               
PRNTRPT  NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         NI    DMINBTS,X'7F'       TURN OFF READ FOR UPDATE                     
         MVI   ERROR,INVALID                                                    
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PRSEQ    DS    0H                                                               
         LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
PR30     DS    0H                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   PRNTRPTX                                                         
*                                                                               
         CLI   RINVKSRC,0          MUST BE A HEADER                             
         BNE   PRSEQ                                                            
         CLI   RINVKINV+3,0        ONLY NEW INVENTORIES ALLOWED                 
         BE    PRSEQ                                                            
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY HEADER KEY                         
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
         OC    DPTHLD,DPTHLD       ANY DAYPART FILTERS?                         
         BZ    PR70                                                             
*                                                                               
         LA    RF,RINVDP           DYPTS IN HDR                                 
         LA    R5,MAXDPT#          MAX # OF DYPTS                               
*                                                                               
PR50     DS    0H                                                               
         CLI   0(RF),0             FOUND DYPT MATCH?                            
         BE    PRSEQ               NO                                           
*                                                                               
         CLC   DPTHLD,0(RF)        MATCHING DYPT?                               
         BE    PR70                YES                                          
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R5,PR50                                                          
         B     PRSEQ               NO MATCHES FOUND                             
*                                                                               
PR70     DS    0H                                                               
         MVC   CODHLD,FCODHLD      GET EQUATE # FOR FROM RATE                   
         MVC   LENHLD,FLENHLD                                                   
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO                                              
         TM    MYFLAG,GOTEQU#      FOUND EQUATE NUMBER?                         
         BO    *+16                NO - GET NEXT FROM HEADER                    
         BAS   RE,PRNTHDR                                                       
         BAS   RE,PRINT                                                         
         B     PRSEQ                                                            
*                                                                               
         MVC   FEQUNUM,EQUNUM      SAVE AWAY FROM EQU#                          
*                                                                               
PR100    DS    0H                                                               
         XC    KEY,KEY             GO GET FROM RATE RECORD                      
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY      COPY KEY UP TO SOURCE                        
*                                                                               
         MVI   RINVKSRC,C'Z'       GAV RATE RECORD                              
         MVC   RINVKNUM,EQUNUM     UNIQUE GAV NUMBER EQUATE                     
         MVC   RINVKYR,FYEARHLD    AVAIL YEAR                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    PR110                                                            
*                                                                               
         XC    KEY,KEY             RATE RECORD IS DELETED                       
         MVC   KEY(27),HDRKEY      - GET NEXT FROM HEADER                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         B     PRSEQ                                                            
*                                                                               
PR110    DS    0H                                                               
         GOTO1 GETREC              GET THE FROM RATE RECORD                     
*                                                                               
         BAS   RE,BLDFCOST         BUILD FROM COSTS TAB (REQ. QTR)              
*                                                                               
         XC    KEY,KEY             RATE RECORD IS DELETED                       
         MVC   KEY(27),HDRKEY      - GET NEXT FROM HEADER                       
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         NI    PROCFLAG,X'FF'-TDPTADJ                                           
         XC    PDPTHLD,PDPTHLD                                                  
*                                                                               
         CLC   TRADJ1D,TRADJ2D     ANY DPT ADJUSTMENTS?                         
         BE    PR140               NO DPT ADJ                                   
*                                                                               
         MVC   TDPTHLD,TRADJ1D     INITIALIZE TO FIRST ADJ                      
         MVC   TDADJHLD,TRADJ1                                                  
*                                                                               
         CLI   TRADJ1D,0                                                        
         BNE   *+16                                                             
         MVC   TDPTHLD,TRADJ2D                                                  
         MVC   TDADJHLD,TRADJ2                                                  
*                                                                               
         LA    RF,RINVDP           DYPTS IN HDR                                 
         LA    R5,MAXDPT#          MAX # OF DYPTS                               
*                                                                               
PR130    DS    0H                                                               
         CLI   0(RF),0             FOUND DYPT MATCH?                            
         BE    PR140               NO                                           
*                                                                               
         CLC   TDPTHLD,0(RF)       MATCHING DYPT?                               
         BNE   PR135                                                            
*                                                                               
         OI    PROCFLAG,TDPTADJ                                                 
         MVC   PDPTHLD,TDPTHLD     FOR REPORT                                   
*                                                                               
         B     PR140                                                            
*                                                                               
PR135    DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   R5,PR130                                                         
*                                                                               
PR140    DS    0H                                                               
         MVC   CODHLD,TCODHLD      GET EQUATE # FOR TO RATE                     
         MVC   LENHLD,TLENHLD                                                   
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO                                              
         TM    MYFLAG,GOTEQU#      FOUND EQUATE NUMBER?                         
         BO    PR145               YES                                          
*                                                                               
* ADD NEW X'06' IN HEADER AND NEW RATE RECORD HERE!!!!                          
*                                                                               
         GOTO1 =A(UPHDR06),RR=RELO                                              
*                                                                               
PR145    DS    0H                                                               
         MVC   TEQUNUM,EQUNUM      SAVE AWAY TO EQU#                            
         LA    R4,TQTRYR           GET TO RATE RECORDS                          
*                                                                               
PR150    DS    0H                                                               
         CLI   0(R4),X'FF'         ANY MORE QTR/YRS?                            
         BE    PR200                                                            
*                                                                               
         MVC   QTRHLD,0(R4)        QTR TO COPY RATES                            
         MVC   YEARHLD,2(R4)       YEAR TO COPY RATES                           
*                                                                               
         GOTO1 =A(ADDRDET),RR=RELO                                              
         GOTO1 =A(COPYRATE),RR=RELO  COPY RATES                                 
*                                                                               
         LA    R4,3(R4)                                                         
         B     PR150                                                            
*                                                                               
PR200    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY      - GET NEXT FROM HEADER                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         TM    PROCFLAG,SUPCOPY                                                 
         BO    *+12                                                             
         BAS   RE,PRNTHDR                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         B     PRSEQ                                                            
*                                                                               
PRNTRPTX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*              PRINT HEADER INFO                               *                
****************************************************************                
PRNTHDR  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,P                                                             
         USING HEADD,R2                                                         
*                                                                               
         MVC   HEADINV(4),RINVKINV    INV #                                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(5,HEADEFF)                             
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BE    PRHDR10                                                          
*                                                                               
         MVI   HEADEFF+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(5,HEADEFF+9)                         
         DROP  R6                                                               
*                                                                               
PRHDR10  DS    0H                  PROGRAM NAME                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         USING RIPGELEM,R6                                                      
*                                                                               
         XC    HEADPGM,HEADPGM                                                  
         ZIC   R1,RIPGLEN                                                       
         SH    R1,=H'2'                                                         
         CH    R1,=H'27'           MAX OUTPUT SIZE                              
         BNL   PRHDR30                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   HEADPGM(0),RIPGNAME                                              
         B     *+10                                                             
*                                                                               
PRHDR30  MVC   HEADPGM,RIPGNAME                                                 
*                                                                               
         MVC   HEADDPT+3(1),PDPTHLD     DYPT ADJ                                
*                                                                               
         CLI   PDPTHLD,0                                                        
         BE    *+8                                                              
         MVI   HEADDPT+4,C'/'                                                   
*                                                                               
         OC    PDADJHLD,PDADJHLD                                                
         BZ    PRHDR40                                                          
*                                                                               
         L     RF,PDADJHLD         DYPT ADJ%                                    
         SR    RE,RE                                                            
*                                                                               
         EDIT  (RF),(7,HEADADJ),ALIGN=LEFT                                      
*                                                                               
PRHDR40  DS    0H                                                               
         MVC   HEADRES,=CL30'COPIED'                                            
         TM    MYFLAG,GOTEQU#      SUCCESSFUL COPY?                             
         BO    PRHDRX              YES                                          
*                                                                               
         MVC   HEADRES,=CL30'FROM RATES DO NOT EXIST'                           
*                                                                               
PRHDRX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*    BUILD TABLE OF FROM COSTS FOR REQUESTED QUARTER                            
***********************************************************************         
BLDFCOST NTR1                                                                   
         L     R6,AIO                                                           
         LA    R5,FQTCOSTS                                                      
*                                                                               
         MVI   ELCODE,X'03'        GET WEEKLY RATE INFO                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
BFCST10  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   BLDFCX                                                           
*                                                                               
         CLC   RIAVQTR,FQTRHLD     THIS REQ. FROM QUARTER?                      
         BNE   BFCST10                                                          
*                                                                               
         MVC   0(4,R5),RIAVAMT     SAVE AWAY COST                               
*                                                                               
         LA    R5,4(R5)                                                         
         MVI   0(R5),X'FF'         DENOTE END OF TABLE                          
         B     BFCST10                                                          
*                                                                               
BLDFCX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R7)                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,36,C'RDETAIL COPY REPORT'                                     
         PSPEC H2,36,C'-------------------'                                     
         PSPEC H2,1,AGYNAME                                                     
         PSPEC H3,1,REQUESTOR                                                   
         PSPEC H4,1,C'STATION:'                                                 
         PSPEC H5,1,C'FROM:'                                                    
         PSPEC H6,1,C'TO:'                                                      
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
*                                                                               
* MOVE HEADINGS INTO H FIELDS                                                   
*                                                                               
         MVC   H2+67(10),=C'REQUESTED:'                                         
         GOTO1 DATCON,DMCB,(5,0),(11,H2+78)                                     
*                                                                               
         MVC   H4+9(L'RNCSSTA),RNCSSTA                                          
*                                                                               
         LA    R3,H5                                                            
         USING FROMD,R3                                                         
*                                                                               
         MVC   FROMCD,=CL5'CODE:'                                               
         MVC   FROMCODE(L'RNCFCOD),RNCFCOD                                      
*                                                                               
         MVC   FROMLN,=CL7'LENGTH:'                                             
         MVC   FROMLEN(L'RNCFLEN),RNCFLEN                                       
*                                                                               
         MVC   FROMQY,=CL7'QTR/YR:'                                             
         MVC   FROMQTYR(L'RNCFQTR),RNCFQTR                                      
*                                                                               
         MVC   FROMDPT,=CL5'DYPT:'                                              
         MVC   FROMDYPT(L'RNCFDPT),RNCFDPT                                      
         DROP  R3                                                               
*                                                                               
         LA    R3,H6                                                            
         USING TOD,R3                                                           
*                                                                               
         MVC   TOCD,=CL5'CODE:'                                                 
         MVC   TOCODE(L'RNCTCOD),RNCTCOD                                        
*                                                                               
         CLC   FCODHLD,TCODHLD                                                  
         BNE   *+10                                                             
         MVC   TOCODE(L'RNCFCOD),RNCFCOD                                        
*                                                                               
         MVC   TOLN,=CL7'LENGTH:'                                               
         MVC   TOLEN(L'RNCTLEN),RNCTLEN                                         
*                                                                               
         CLC   FLENHLD,TLENHLD                                                  
         BNE   *+10                                                             
         MVC   TOLEN(L'RNCFLEN),RNCFLEN                                         
*                                                                               
         MVC   TOQY,=CL7'QTR/YR:'                                               
         MVC   TOQTYR(L'RNCTQTR),RNCTQTR                                        
*                                                                               
         CLI   RNCTQTRH+5,0                                                     
         BNE   *+10                                                             
         MVC   TOQTYR(L'RNCFQTR),RNCFQTR                                        
*                                                                               
         LA    R3,H7                                                            
         MVC   TOCD,=CL5'ADJ%:'                                                 
         MVC   TOCODE(L'RNCRADJ),RNCRADJ                                        
         DROP  R3                                                               
*                                                                               
         LA    R3,H8                                                            
         USING HEADD,R3                                                         
*                                                                               
         MVC   HEADINV,=CL6'INV #'                                              
         MVC   HEADPGM,=CL27'PROGRAM'                                           
         MVC   HEADEFF,=CL17'EFF DATE'                                          
         MVC   HEADDPT(5),=CL5'DYPT/'                                           
         MVC   HEADADJ,=CL4'ADJ%'                                               
         MVC   HEADRES,=CL30'RESULTS'                                           
*                                                                               
         LA    R3,H9                                                            
         MVC   HEADINV,=CL8'--------'                                           
         MVC   HEADPGM,=CL27'---------------------------'                       
         MVC   HEADEFF,=CL17'-----------------'                                 
         MVC   HEADDPT(5),=CL5'-----'                                           
         MVC   HEADADJ,=CL4'----'                                               
         MVC   HEADRES,=CL30'------------------------------'                    
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
PRREPF   DS    CL8'REPFILE'                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*    COPY RATES TO "TO" RECORD                                                  
***********************************************************************         
COPYRATE NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             GO GET FROM RATE RECORD                      
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY      COPY KEY UP TO SOURCE                        
*                                                                               
         MVI   RINVKSRC,C'Z'       GAV RATE RECORD                              
         MVC   RINVKNUM,TEQUNUM     UNIQUE GAV NUMBER EQUATE                    
         MVC   RINVKYR,YEARHLD      AVAIL YEAR                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R5,FQTCOSTS         COSTS FOR FROM REQ. QTR                      
*                                                                               
         MVI   ELCODE,X'03'        GET WEEKLY RATE INFO                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RIAVL,R6                                                         
*                                                                               
CR10     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CR100                                                            
*                                                                               
         CLI   0(R5),X'FF'         END OF FROM COSTS?                           
         BE    CR100                                                            
*                                                                               
         CLC   RIAVQTR,QTRHLD      THIS REQ. TO QUARTER?                        
         BNE   CR10                                                             
*                                                                               
         TM    OPTFLAG,TRTADJ      ANY RATE ADJUSTMENTS?                        
         BO    *+14                YES                                          
         MVC   RIAVAMT,0(R5)       COPY RATE FOR CORRESPONDING WEEK             
         B     CR80                                                             
*                                                                               
CR20     DS    0H                                                               
         XC    ADJHLD,ADJHLD                                                    
         XC    PDADJHLD,PDADJHLD                                                
*                                                                               
         TM    PROCFLAG,TDPTADJ    DO DPT ADJ FOR THIS QTR?                     
         BO    CR30                                                             
*                                                                               
         MVC   ADJHLD,TRADJ1       INITIALIZE TO 1ST ADJ                        
         CLI   TRADJ1D,0                                                        
         BE    CR50                                                             
*                                                                               
         MVC   ADJHLD,TRADJ2                                                    
         CLI   TRADJ2D,0                                                        
         JNE   ERREND                                                           
         B     CR50                                                             
*                                                                               
CR30     DS    0H                                                               
         MVC   ADJHLD,TRADJ1       INITIALIZE TO 1ST ADJ                        
         CLI   TRADJ1D,0                                                        
         BNE   CR50                                                             
*                                                                               
         MVC   ADJHLD,TRADJ2                                                    
         CLI   TRADJ2D,0                                                        
         JE    ERREND                                                           
*                                                                               
CR50     DS    0H                                                               
         ICM   RF,15,0(R5)                                                      
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
*                                                                               
         CLI   ADJHLD+3,X'64'                                                   
         BE    *+10                                                             
         MVC   PDADJHLD,ADJHLD                                                  
*                                                                               
         OC    ADJHLD,ADJHLD       ANY ADJ %                                    
         BNZ   *+8                                                              
         MVI   ADJHLD+3,X'64'      DEFAULT TO 100 %                             
*                                                                               
         ICM   R4,15,ADJHLD                                                     
         MR    RE,R4                                                            
*                                                                               
         STCM  RF,15,RIAVAMT                                                    
*                                                                               
CR80     DS    0H                                                               
         LA    R5,4(R5)                                                         
         B     CR10                                                             
*                                                                               
CR100    DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO UPDATE RATE CARD                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET ORIGINAL HEADER BACK                     
*                                                                               
CRX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*       GET RATE ADJUSTMENTS                                                    
*****************************************************************               
GETADJ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 SCANNER,DMCB,(0,RNCRADJH),WORK,                                  
         CLI   DMCB+4,0            DID IT WORK?                                 
         JE    ERREND              NO                                           
*                                                                               
         XC    TRADJ1D,TRADJ1D                                                  
         XC    TRADJ1,TRADJ1                                                    
         XC    TRADJ2D,TRADJ2D                                                  
         XC    TRADJ2,TRADJ2                                                    
*                                                                               
         LA    R3,WORK                                                          
         LA    R5,TRADJ1D                                                       
*                                                                               
GETAD10  DS    0H                                                               
         CLI   0(R3),0             ANY MORE ADJS?                               
         BE    GETAD50                                                          
*                                                                               
         LA    R4,12(R3)                                                        
         ZIC   R1,0(R3)                                                         
         AHI   R1,-1                                                            
         JNP   ERREND                                                           
*                                                                               
         AR    R4,R1                                                            
         CLI   0(R4),C'%'          CHECK FOR % SIGN                             
         JNE   ERREND                                                           
*                                                                               
         LA    R4,12(R3)                                                        
         CLI   0(R4),C'0'          ANY DPT ADJ?                                 
         BNL   GETAD20                                                          
*                                                                               
         MVC   0(1,R5),0(R4)       SAVE AWAY DPT ADJ                            
         LA    R4,1(R4)                                                         
         AHI   R1,-1               SUBTRACT OFF FOR DPT ADJ                     
         JNP   ERREND                                                           
*                                                                               
GETAD20  DS    0H                                                               
         XC    DUMMYHDR,DUMMYHDR                                                
         MVI   DUMMYHDR,X'0B'      TOTAL LEN OF 11                              
         STC   R1,DUMMYHDR+5       LEN OF NUMBER                                
*                                                                               
         AHI   R1,-1                                                            
         JNP   ERREND                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUMMYHDR+8(0),0(R4)                                              
*                                                                               
         LA    R5,1(R5)                                                         
*                                                                               
         LA    R2,DUMMYHDR                                                      
         OI    1(R2),X'10'         NUMERIC                                      
         OI    4(R2),X'08'         VALID NUMERIC                                
*                                                                               
         GOTO1 VPACK                                                            
         LR    RF,R0                                                            
         STCM  RF,15,0(R5)                                                      
*                                                                               
         LA    R5,4(R5)                                                         
         LA    R3,32(R3)                                                        
*                                                                               
         B     GETAD10                                                          
*                                                                               
GETAD50  DS    0H                                                               
         LA    R2,RNCRADJH                                                      
         CLI   TRADJ1D,0           ANY DPT ADJ?                                 
         BE    GETAD60                                                          
         CLI   TRADJ2D,0           CAN ONLY HAVE 1 DPT ADJ                      
         JNE   ERREND                                                           
         B     GETADJX                                                          
*                                                                               
GETAD60  DS    0H                                                               
         OC    TRADJ2,TRADJ2       ANY 2ND ADJ?                                 
         BZ    GETADJX                                                          
         CLI   TRADJ2D,0                                                        
         JE    ERREND                                                           
*                                                                               
GETADJX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*       ADD X'06' ELEMENT IN HEADER FOR THIS RDETAIL                            
*****************************************************************               
UPHDR06  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           BUILD WEEKLY ELEM W/ COST                    
         LA    R4,ELEM                                                          
         USING RIMAELEM,R4                                                      
*                                                                               
         MVI   RIMACODE,X'06'      ELEM TYPE                                    
         MVI   RIMALEN,RIMALENQ    ELEM LENGTH                                  
         MVC   RIMANUM,EQUNUM      EQUATE #                                     
         MVC   RIMAREP,REPHLD      REP CODE                                     
         MVC   RIMACDE,TCODHLD     RATE CODE                                    
         MVC   RIMALNTH,TLENHLD    SPOT LENGTH                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',UPHDREPF),(0,AIO),(R4),0                        
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 =A(UPDRTCD),RR=RELO UPDATE RATE CARD                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET ORIGINAL HEADER BACK                     
*                                                                               
UPHDR06X DS    0H                                                               
         XIT1                                                                   
*                                                                               
UPHDREPF DC    CL8'REPFILE'                                                     
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        ADD NEW 'Z' RECORD W/ NEW RATE                                         
*        INPUT   AIO MUST HAVE HEADER RECORD                                    
*****************************************************************               
ADDRDET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =A(BLDBRDT),RR=RELO                                              
*                                                                               
         L     RF,AIO                                                           
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),HDRKEY                                                   
         MVI   RINVKSRC,C'Z'       RDETAIL RECORD TYPE                          
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,YEARHLD     YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    ADDRD60             'Z' REC ALREADY EXISTS                       
*                                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         L     R6,AIO              BUILD NEW 'Z' RECORD                         
         XCEF  (R6),2000           CLEAR                                        
*                                                                               
         MVC   0(27,R6),KEYSAVE                                                 
*                                                                               
         MVC   RINVLEN,=H'34'      REC LENGTH SO FAR                            
*                                                                               
         LA    R3,BRDTAB                                                        
         USING BROADD,R3                                                        
*                                                                               
         MVC   TMPBRDCU,BRDSTART   INTIALIZE W/ FIRST WEEK OF YEAR              
*                                                                               
ADDRD10  DS    0H                                                               
         XC    ELEM,ELEM           BUILD WEEKLY ELEM W/ COST                    
         LA    R4,ELEM                                                          
         USING RIAVL,R4                                                         
         MVI   RIAVCODE,X'03'      ELEM TYPE                                    
         MVI   RIAVLEN,RIAVLENQ    ELEM LENGTH                                  
*                                                                               
         CLC   TMPBRDCU,ENBRDYRJ   FINISHED W/ THIS BRD YEAR?                   
         BH    ADDRD50             YES - ADD ACTIVITY ELEMENT                   
*                                                                               
         CLC   TMPBRDCU,BRDEND     PAST LAST WEEK IN THIS QTR?                  
         BNH   *+12                                                             
         LA    R3,L'BRDTAB(R3)     BUMP TO NEXT QTR                             
         B     ADDRD10                                                          
*                                                                               
         MVC   RIAVQTR,BRDQTR      QUARTER #                                    
         MVC   RIAVWEEK,TMPBRDCU   BROADCAST WEEK                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',ADDRREPF),(0,AIO),(R4),0                        
*                                                                               
*  GET NEXT BROADCAST WEEK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(8,TMPBRDCU),(0,TMPDTEE)                             
*                                                                               
         LA    R5,7                                                             
         GOTO1 ADDAY,DMCB,TMPDTEE,TMPDTEE2,(R5)                                 
         GOTO1 DATCON,DMCB,(0,TMPDTEE2),(19,TMPBRDCU)                           
         B     ADDRD10                                                          
         DROP  R4                                                               
*                                                                               
ADDRD50  DS    0H                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING RINVAEL,R4                                                       
*                                                                               
         MVI   RINVACOD,X'EF'      ELEM CODE                                    
         MVI   RINVALEN,RINVALNQ   ELEM LENGTH                                  
         GOTO1 DATCON,DMCB,(5,0),(3,RINVAFST)                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',ADDRREPF),(0,AIO),(R4),0                        
*                                                                               
         GOTO1 ADDREC              ADD NEW RATE RECORD                          
*                                                                               
ADDRD60  DS    0H                                                               
         GOTO1 =A(UPDRTCD),RR=RELO UPDATE RATE CARD                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET ORIGINAL HEADER BACK                     
*                                                                               
ADDRDETX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
ADDRREPF DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*****************************************************************               
*  BUILD TABLE W/ START AND END BRD WEEKS FOR EACH QUARTER                      
*****************************************************************               
BLDBRDT  NTR1  BASE=*,LABEL=*                                                   
         XC    BRDTAB(BRDTABLN),BRDTAB                                          
*                                                                               
         LA    R3,BRDTAB                                                        
         USING BROADD,R3                                                        
         LA    R4,QTRTABX                                                       
*                                                                               
BLDBR10  DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    BLDBR20                                                          
*                                                                               
         MVC   BRDQTR,0(R4)        QUARTER #                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+1(2),1(R4)     START BRD DATE RANGE                         
         MVC   WORK+3(1),YEARHLD                                                
         MVC   WORK+4(2),3(R4)     END BRD DATE RANGE                           
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,BRDSTART)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,BRDEND)                            
*                                                                               
         LA    R3,BROADDLN(R3)                                                  
         LA    R4,QTRTABXL(R4)                                                  
         B     BLDBR10                                                          
*                                                                               
BLDBR20  DS    0H                                                               
*                                                                               
         XC    WORK,WORK           GET FIRST AND LAST WEEK OF BRD YEAR          
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+4(2),=X'0C0F'                                               
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,STBRDYRJ)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,ENBRDYRJ)                          
*                                                                               
BLDBRDTX DS    0H                                                               
         MVI   0(R3),X'FF'         DENOTE END OF TABLE                          
         XIT1                                                                   
*                                                                               
QTRTABX  DC    XL1'01',XL4'010F030F'                                            
         DC    XL1'02',XL4'040F060F'                                            
         DC    XL1'03',XL4'070F090F'                                            
         DC    XL1'04',XL4'0A0F0C0F'                                            
         DC    XL1'FF'                                                          
QTRTABXL EQU   5                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET EQUATE NUMBER FOR 'Z' RECORD                                       
*        INPUT    AIO MUST HAVE HEADER RECORD                                   
*        OUTPUT   EQUNUM - EQUATE NUMBER FOR 'Z' RECORD                         
*        OUTPUT   MYFLAG - OI W/ GOTEQU# IF 'Z' RECORD EXISTS                   
*****************************************************************               
GETEQU#  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        MASTER AVAIL ELEMENT                         
*                                                                               
         NI    MYFLAG,X'FF'-GOTEQU#                                             
*                                                                               
         LA    R5,1                                                             
         STC   R5,EQUNUM                                                        
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETEQ10  DS    0H                                                               
         BRAS  RE,NEXTEL           FOUND EQUATE NUMBER IN RECORD?               
         BNE   GETEQU#X            NO                                           
         USING RIMAELEM,R6                                                      
*                                                                               
         CLC   RIMAREP,REPHLD      SAME REP (CHILD)?                            
         BNE   GETEQ20             NO                                           
         CLC   RIMACDE,CODHLD      SAME AVAIL CODE?                             
         BNE   GETEQ20             NO                                           
         CLC   RIMALNTH,LENHLD     SAME AVAIL LENGTH?                           
         BNE   GETEQ20             NO                                           
*                                                                               
         MVC   EQUNUM,RIMANUM      FOUND EQUATE NUMBER                          
         OI    MYFLAG,GOTEQU#                                                   
         B     GETEQU#X                                                         
*                                                                               
GETEQ20  DS    0H                                                               
         CLC   EQUNUM,RIMANUM                                                   
         BNE   *+12                                                             
         LA    R5,1(R5)                                                         
         STC   R5,EQUNUM                                                        
         B     GETEQ10                                                          
*                                                                               
GETEQU#X DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE STATION                                                       
*****************************************************************               
VALSTA   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,MISSING                                                    
         L     R2,0(R1)                                                         
*                                                                               
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
         GOTO1 VALISTA                                                          
*                                                                               
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*        VALIDATE LENGTH                                                        
*****************************************************************               
VALLEN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         XC    LENHLD,LENHLD                                                    
*                                                                               
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         LR    RE,R2                                                            
         LA    RE,8(RE)            POINT TO FIELD                               
         ZIC   RF,5(R2)                                                         
         ZIC   R1,5(R2)                                                         
*                                                                               
VL10     CLI   0(RE),C'0'          MUST BE NUMBERIC                             
         BL    VL20                                                             
         CLI   0(RE),C'9'                                                       
         BH    VL20                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,VL10                                                          
         B     VL30                                                             
*                                                                               
VL20     MVI   ERROR,INVALID                                                    
         C     RF,=F'1'            IF NOT IN LAST POSITION ERROR                
         JNE   ERREND                                                           
         BCTR  R1,0                CHECK FOR MINUTES/SECONDS INDICATOR          
         CLI   0(RE),C'M'                                                       
         BNE   *+12                                                             
         OI    LENHLD,X'80'                                                     
         B     VL30                                                             
         CLI   0(RE),C'S'                                                       
         JNE   ERREND                                                           
*                                                                               
VL30     BCTR  R1,0                CONVERT TO BINARY                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,LENHLD+1                                                    
*                                                                               
VALLENX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET QTR/YEAR                                                           
*****************************************************************               
GETQTYR  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,MISSING                                                    
*                                                                               
         L     R2,0(R1)                                                         
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,QTRTABA                                                       
         LA    RF,4                                                             
*                                                                               
GQY10    DS    0H                  CHECK IF QUARTER IS IN TABLE                 
         MVI   ERROR,INVALID                                                    
         CLC   8(2,R2),0(RE)       IN TABLE?                                    
         BE    GQY20                                                            
*                                                                               
         LA    RE,QTRTABAL(RE)     BUMP TO NEXT ENTRY                           
         BCT   RF,GQY10                                                         
         J     ERREND                                                           
*                                                                               
GQY20    DS    0H                                                               
         MVC   QTRHLD,1(RE)                                                     
         NI    QTRHLD,X'0F'        MAKE IT BINARY                               
*                                                                               
         MVC   WORK+1(2),2(RE)     START BRD DATE RANGE                         
         MVC   WORK+4(2),4(RE)     END BRD DATE RANGE                           
         MVC   QTRBIT,6(RE)        QTR BIT                                      
*                                                                               
         CLI   10(R2),C'/'                                                      
         JNE   ERREND                                                           
*                                                                               
         MVC   HALF,11(R2)         CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GQY30    CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,GQY30                                                         
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   DUB(2),11(R2)       MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
GETQYX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
QTRTABA  DC    CL2'Q1',XL5'010F030F80'                                          
         DC    CL2'Q2',XL5'040F060F40'                                          
         DC    CL2'Q3',XL5'070F090F20'                                          
         DC    CL2'Q4',XL5'0A0F0C0F10'                                          
QTRTABAL EQU   7                                                                
         EJECT                                                                  
*****************************************************************               
*        GET QTR/YEAR FOR TO DETAILS                                            
*****************************************************************               
GETTQTYR NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,MISSING                                                    
*                                                                               
         L     R2,0(R1)                                                         
         LA    R3,TQTRYR                                                        
         LA    R4,8(R2)                                                         
*                                                                               
GTQY05   DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RE,TQTRTABA                                                      
         LA    RF,4                                                             
*                                                                               
GTQY10   DS    0H                  CHECK IF QUARTER IS IN TABLE                 
         MVI   ERROR,INVALID                                                    
         CLC   0(2,R4),0(RE)       IN TABLE?                                    
         BE    GTQY20                                                           
*                                                                               
         LA    RE,TQTRTABL(RE)     BUMP TO NEXT ENTRY                           
         BCT   RF,GTQY10                                                        
         J     ERREND                                                           
*                                                                               
GTQY20   DS    0H                                                               
         MVC   0(1,R3),1(RE)       MOVE IN QTR                                  
         NI    0(R3),X'0F'         MAKE IT BINARY                               
*                                                                               
         MVC   WORK+1(2),2(RE)     START BRD DATE RANGE                         
         MVC   WORK+4(2),4(RE)     END BRD DATE RANGE                           
         MVC   1(1,R3),6(RE)       QTR BIT                                      
*                                                                               
         CLI   2(R4),C'/'                                                       
         JNE   ERREND                                                           
*                                                                               
         MVC   HALF,3(R4)          CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GTQY30   CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,GTQY30                                                        
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   DUB(2),3(R4)        MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   2(1,R3),TMPDTEE     YEAR IN BINARY                               
*                                                                               
         MVI   3(R3),X'FF'         DENOTE END OF TABLE                          
         LA    R3,3(R3)            NEXT ENTRY IN TQTRYR                         
*                                                                               
         LA    R4,5(R4)            NEXT QTR/YR INPUT                            
         CLI   0(R4),0             ANY MORE                                     
         BE    GETTQYX                                                          
         CLI   0(R4),C','          SHOULD BE A COMMA                            
         JNE   ERREND                                                           
         LA    R4,1(R4)                                                         
*                                                                               
         B     GTQY05                                                           
*                                                                               
GETTQYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
TQTRTABA DC    CL2'Q1',XL5'010F030F80'                                          
         DC    CL2'Q2',XL5'040F060F40'                                          
         DC    CL2'Q3',XL5'070F090F20'                                          
         DC    CL2'Q4',XL5'0A0F0C0F10'                                          
TQTRTABL EQU   7                                                                
         EJECT                                                                  
*****************************************************************               
*        UPDATE RATE CODE KEY ENTERED (3E)                                      
*****************************************************************               
UPDRTCD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,DUB),(19,TODAYJ)                                  
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RTCDKEY,KEY         SAVE AWAY RATE RECORD KEY                    
*                                                                               
         LA    R2,RNCFCODH         SET UP HELLO LOOKUP INFO                     
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',UPDREPF),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
*                                                                               
         LA    R6,RALQLST1         ENTER LAST DATE CHANGED                      
         ZIC   R5,QTRHLD                                                        
         B     *+8                                                              
*                                                                               
UPDR10   DS    0H                                                               
         LA    R6,3(R6)            BUMP TO NEXT QTR                             
         BCT   R5,UPDR10                                                        
*                                                                               
         MVC   0(3,R6),TODAYJ                                                   
         DROP  R6                                                               
*                                                                               
UPDR20   DS    0H                  CHECK/ADD NEW X'03' ELEMENT                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
         USING RASTELEM,R6                                                      
*                                                                               
UPDR30   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   UPDR100             DOESN'T EXIST - ADD NEW ONE                  
*                                                                               
         CLC   RASTSTA,STAHLD      SAME SATION?                                 
         BNE   UPDR30                                                           
*                                                                               
         XC    ELEM,ELEM           FOUND ELEMENT, UPDATE AND PUT BACK           
         ZIC   R1,RASTLEN                                                       
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       SAVE AWAY ELEMENT                            
*                                                                               
         MVI   0(R6),X'FF'         MARK THIS ELEMENT FOR DELETION               
         GOTO1 HELLO,DMCB,(C'D',UPDREPF),(X'FF',AIO),0                          
*                                                                               
         LA    R6,ELEM                                                          
         ZIC   R1,RASTLEN                                                       
         LA    RF,RASTLENQ                                                      
         SR    R1,RF               # OF YEARS                                   
         BP    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,RASTYR                                                        
*                                                                               
UPDR40   DS    0H                                                               
         CLC   0(1,R6),YEARHLD     YEAR ALREADY HERE ?                          
         BE    UPDR130             YES - PUT BACK ELEMENT                       
*                                                                               
         LA    R6,1(R6)                                                         
         BCT   R1,UPDR40                                                        
*                                                                               
         MVC   0(1,R6),YEARHLD     ADD YEAR IN ELEMENT                          
*                                                                               
         LA    R6,ELEM                                                          
         ZIC   R1,RASTLEN                                                       
         LA    R1,1(R1)            INCREMENT LEN BY 1 FOR NEW YEAR              
         STC   R1,RASTLEN                                                       
         B     UPDR130             UPDATE RECORD WITH ELEMENT                   
*                                                                               
UPDR100  DS    0H                  ADD NEW X'03' ELEMENT FOR THIS STA           
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         MVI   RASTCODE,X'03'                                                   
*                                                                               
         LA    R1,RASTLENQ                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RASTLEN                                                       
*                                                                               
         MVC   RASTSTA,STAHLD      STATION                                      
         MVC   RASTCDTE,TODAYJ     ELEMENT CREATE DATE (TODAY)                  
*                                                                               
         LA    R6,RASTYR                                                        
         MVC   0(1,R6),YEARHLD                                                  
*                                                                               
UPDR130  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',UPDREPF),(0,AIO),ELEM,0                         
*                                                                               
UPDRTCDX DS    0H                                                               
         GOTO1 PUTREC                                                           
         XIT1                                                                   
*                                                                               
UPDREPF  DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RATE CODE KEY ENTERED (3E)                                    
*****************************************************************               
CHKRTCD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         L     R2,0(R1)                                                         
         ST    R2,ACURFLD                                                       
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BNE   RTCDNFND                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RTCDKEY,KEY         SAVE AWAY RATE RECORD KEY                    
*                                                                               
* CHECK IF STATION IS ACTIVE                                                    
*                                                                               
         MVI   ELCODE,X'04'                                                     
*                                                                               
CHKRT05  DS    0H                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKRT10  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CHKRT15                                                          
*                                                                               
         CLC   2(5,R6),STAHLD      STATION INACTIVE OR PURGED?                  
         BNE   CHKRT10             CHECK NEXT ELEMENT                           
*                                                                               
         CLI   ELCODE,X'05'                                                     
         BNE   INACTERR                                                         
*                                                                               
         TM    18(R6),X'01'        RESTORED?                                    
         BO    CHKRT20             YES                                          
         B     INACTERR                                                         
*                                                                               
CHKRT15  DS    0H                                                               
         CLI   ELCODE,X'05'                                                     
         BE    CHKRT20                                                          
         MVI   ELCODE,X'05'                                                     
         B     CHKRT05                                                          
*                                                                               
CHKRT20  DS    0H                                                               
         BRAS  RE,NEXTFLD                                                       
*                                                                               
         TM    PROCFLAG,TRATE      PROCESSING TO RATE DETAILS?                  
         BZ    CHKR50                                                           
         LA    R3,TQTRYR                                                        
*                                                                               
CHKR40   DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TO QTRS                               
         BE    CHKRTCDX                                                         
         MVC   YEARHLD,2(R3)       MOVE IN YEAR TO CHECK FOR                    
*                                                                               
CHKR50   DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',CHKREPF),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BNE   RTCDNFND                                                         
*                                                                               
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
*                                                                               
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),QTRBIT       OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BNE   RTCDNFND                                                         
*                                                                               
         TM    PROCFLAG,TRATE      PROCESSING TO DETAILS                        
         BZ    CHKRTCDX                                                         
         LA    R3,3(R3)            BUMP TO NEXT QTR FILTER                      
         B     CHKR40                                                           
*                                                                               
CHKRTCDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
RTCDNFND DS    0H                                                               
         L     R2,ACURFLD          RATE RECORD NOT FOUND                        
         MVC   RERROR(2),=AL2(RATENFND)                                         
         GOTO1 MYERROR                                                          
*                                                                               
INACTERR DS    0H                                                               
         L     R2,ACURFLD          STATION NOT ACTIVE FOR RATE CARD             
         MVC   RERROR(2),=AL2(NOTACTIV)                                         
         GOTO1 MYERROR                                                          
*                                                                               
INVRQTRE DS    0H                                                               
         L     R2,ACURFLD          INVALID QTR FOR THIS RATE REC                
         MVC   RERROR(2),=AL2(INVRQTR)                                          
         GOTO1 MYERROR                                                          
*                                                                               
NOTOFLD  DS    0H                                                               
         LA    R2,RNCTCODH         MUST ENTER ONE 'TO' FIELD                    
         MVC   RERROR(2),=AL2(MISSNGTO)                                         
         GOTO1 MYERROR                                                          
*                                                                               
CHKREPF  DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGENINVA                                                                     
* REGENARTE                                                                     
* REGAVWORKD                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGAVFFD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVD5D          RDETAIL/COPY SCREEN                          
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
*                                                                               
       ++INCLUDE REGENARTE                                                      
       ++INCLUDE REGAVWORKD                                                     
*                                                                               
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
*****************************************************                           
*              WORK AREA                                                        
*****************************************************                           
RELO     DS    A                                                                
*                                                                               
DUMMYHDR DS    XL20                DUMMY HEADER FOR VPACK                       
*                                                                               
FQTCOSTS DS    XL60                FROM COSTS FROM REQ. QTR                     
*                                                                               
PROCFLAG DS    XL1                 CURRENT PROCESSING                           
FRATE    EQU   X'01'               PROCESSING FROM DETAILS                      
TRATE    EQU   X'02'               PROCESSING TO DETAILS                        
TDPTADJ  EQU   X'04'               DO DPT ADJ FOR THIS TO RECORD                
SUPCOPY  EQU   X'08'               SUPPRESS SUCCESSFUL COPIED IN REPORT         
*                                                                               
OPTFLAG  DS    XL1                 OPTION FLAGS                                 
TCODE    EQU   X'01'               NEW TO CODE                                  
TLEN     EQU   X'02'               NEW TO LENGTH                                
TQTR     EQU   X'04'               NEW TO QTRS                                  
TRTADJ   EQU   X'08'               NEW TO RATE ADJUSTMENT                       
*                                                                               
SAVEKEY  DS    CL27                                                             
HDRKEY   DS    CL27                HEADER KEY                                   
FKEY     DS    CL27                CURRENT SOURCE HDRKEY                        
FRKEY    DS    CL27                CURRENT SOURCE RATE KEY                      
TOKEY    DS    CL27                CURRENT DESTINATION HDRKEY                   
TORKEY   DS    CL27                CURRENT DESTINATION RATE KEY                 
RTCDKEY  DS    CL27                RATE CODE KEY (3E)                           
*                                                                               
STAHLD   DS    CL5                 STATION                                      
REPHLD   DS    CL2                 REP                                          
CODHLD   DS    CL8                 RATE CODE                                    
LENHLD   DS    XL2                 SPOT LENGTH                                  
YEARHLD  DS    XL1                 YEAR IN BINARY                               
QTRHLD   DS    CL1                 QTR (BINARY)                                 
QTRBIT   DS    XL1                 QUARTER BIT                                  
DPTHLD   DS    CL1                 FROM DETAILS DAYPART FILTER                  
ADJHLD   DS    XL4                 ADJUSTMENT                                   
*                                                                               
* FROM STAION DETAILS                                                           
*                                                                               
FCODHLD  DS    CL8                 RATE CODE                                    
FLENHLD  DS    XL2                 SPOT LENGTH                                  
FYEARHLD DS    XL1                 YEAR IN BINARY                               
FQTRHLD  DS    CL1                 QTR (BINARY)                                 
FQTRBIT  DS    XL1                 QUARTER BIT                                  
*                                                                               
* TO STAION DETAILS                                                             
*                                                                               
TCODHLD  DS    CL8                 RATE CODE                                    
TLENHLD  DS    XL2                 SPOT LENGTH                                  
TQTRYR   DS    XL13                4 SETS (QTR(B),QTR BIT(B),YEAR(B))           
*                                                                               
TRADJ1D  DS    CL1                 DPT RATE ADJ (1ST ADJ)                       
TRADJ1   DS    XL4                 FIRST RATE ADJUSTMENT                        
TRADJ2D  DS    CL1                 DPT RATE ADJ (2ND ADJ)                       
TRADJ2   DS    XL4                 SECOND RATE ADJUSTMENT                       
*                                                                               
TDPTHLD  DS    CL1                 TO DETAILS DAYPART ADJUSTMENT                
TDADJHLD DS    XL4                 TO DPT ADJ RATE                              
*                                                                               
* FOR REPORT                                                                    
*                                                                               
PDPTHLD  DS    CL1                 TO DETAILS DAYPART ADJUSTMENT                
PDADJHLD DS    XL4                 TO DPT ADJ RATE                              
*                                                                               
TODAYJ   DS    XL3                 TODAY'S DATE (JULIAN)                        
*                                                                               
STENDQTR DS    XL4                 DATE RANGE FOR QTR (COMPRESSED)              
STBRDYRJ DS    XL3                 FIRST WEEK OF BRD YEAR (JULIAN)              
ENBRDYRJ DS    XL3                 LAST WEEK OF BRD YEAR (JULIAN)               
STBRDYRC DS    XL2                 FIRST WEEK OF BRD YEAR (COMPRESSED)          
ENBRDYRC DS    XL2                 LAST WEEK OF BRD YEAR (COMPRESSED)           
*                                                                               
EQUNUM   DS    XL1                 EQUATE NUMBER                                
FEQUNUM  DS    XL1                 EQUATE NUMBER FOR 'Z' RECORD                 
TEQUNUM  DS    XL1                 EQUATE NUMBER FOR 'Z' RECORD                 
DEFCOST  DS    F                   RATE                                         
PRVCOST  DS    F                   PREVIOUS COST                                
*                                                                               
ACURFLD  DS    A                   A(CURRENT FIELD ON SCREEN)                   
*                                                                               
KEYTAB   DS    15CL18              KEY HOLD AREA 15 KEYS 18 BYTES EACH          
KEYTABLN EQU   *-KEYTAB                                                         
         DC    X'FF'                                                            
*                                                                               
BRDTAB   DS    4XL7                BROADCAST STR AND END DATES                  
BRDTABLN EQU   *-BRDTAB            FOR 4 QTRS IN YEAR                           
         DC    X'FF'                                                            
*                                                                               
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
THISLINE DS    A                   CURRENT LINE ADDRESS                         
*                                                                               
CURSCRN  DS    XL1                 CURRENT SCREEN                               
LISTSCRN EQU   X'D3'               LISTING SCREEN                               
REPSCRN  EQU   X'D4'               REPORT SCREEN                                
WEEKSCRN EQU   X'D2'               WEEKLY SCREEN                                
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
GOTEQU#  EQU   X'01'               GOT EQUATE # FROM RECORD                     
*                                                                               
TEMP     DS    F                                                                
*                                                                               
TMPDTEE  DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEE2 DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEJ  DS    XL3                 TEMP DATE (JULIAN)                           
TMPBRD   DS    CL12                TEMP BROADCAST DATE RANGE (YYMMDD)           
TMPBRDST DS    XL3                 TEMP START BROADCAST WEEK (JULIAN)           
TMPBRDEN DS    XL3                 TEMP END BROADCAST WEEK (JULIAN)             
TMPBRDCU DS    XL3                 TEMP CURRENT BROADCAST WEEK (JULIAN)         
*                                                                               
BRDDTEE  DS    CL12                BROADCAST DATE RANGE (YYMMDD)                
BRDQTRST DS    XL3                 BROADCAST START DATE (JULIAN)                
BRDQTREN DS    XL3                 BROADCAST END DATE (JULIAN)                  
*                                                                               
HDRSTRTJ DS    XL3                 HEADER START (JULIAN)                        
HDRENDJ  DS    XL3                 HEADER END (JULIAN)                          
*                                                                               
DYPTFILT DS    CL1                 DAYPART FILTER                               
WEEKFILT DS    XL3                 WEEKLY FILTER (JULIAN)                       
INV#FILT DS    CL4                 INV# START AT FILTER                         
*                                                                               
DYPTRFLT DS    CL6                 DAYPART FILTER FOR REPORT                    
QTRRFILT DS    XL9                 QUARTERS REQUESTED FOR REPORT                
INVRFILT DS    CL8                 INV # FILTERS FOR REPORT                     
CURQTFLT DS    XL1                 CURRENT QTR BEING FILTERED                   
*                                                                               
ANXTQTR  DS    A                   A(NEXT QTR IN QTR FILTERS REPORT)            
*                                                                               
MAXLST#  EQU   15                  MAX NUMBER OF LIST ENTRIES                   
MAXDPT#  EQU   6                   MAX NUMBER OF DPTS FOR REPORT                
*                                                                               
*****************************************************                           
*      ERROR EQUATES                                                            
*****************************************************                           
RATENFND EQU   814                 RATE RECORD NOT FOUND FOR COMBO              
INITQTR  EQU   815                 INITIALIZE QTR W/ COST                       
INVRQTR  EQU   825                 INVALID QUARTER FOR THIS RATE REC            
MISSNGTO EQU   833                 MUST ENTER AT LEAST ONE 'TO' FIELD           
NOTACTIV EQU   845                 STA NOT ACTIVE FOR RATE CODE                 
*                                                                               
*****************************************************                           
*      DSECT FOR TO INFORMATION                                                 
*****************************************************                           
TOD      DSECT                                                                  
         DS    CL8                                                              
TOCD     DS    CL5                 CODE                                         
         DS    CL1                                                              
TOCODE   DS    CL8                                                              
         DS    CL5                                                              
TOLN     DS    CL7                 LENGTH                                       
         DS    CL1                                                              
TOLEN    DS    CL4                                                              
         DS    CL5                                                              
TOQY     DS    CL7                 QTR/YR                                       
         DS    CL1                                                              
TOQTYR   DS    CL30                                                             
*                                                                               
*****************************************************                           
*      DSECT FOR FROM INFORMATION                                               
*****************************************************                           
FROMD    DSECT                                                                  
         DS    CL8                                                              
FROMCD   DS    CL5                 CODE                                         
         DS    CL1                                                              
FROMCODE DS    CL8                                                              
         DS    CL5                                                              
FROMLN   DS    CL7                 LENGTH                                       
         DS    CL1                                                              
FROMLEN  DS    CL4                                                              
         DS    CL5                                                              
FROMQY   DS    CL7                 QTR/YR                                       
         DS    CL1                                                              
FROMQTYR DS    CL5                                                              
         DS    CL10                                                             
FROMDPT  DS    CL5                                                              
         DS    CL1                                                              
FROMDYPT DS    CL1                                                              
*                                                                               
*****************************************************                           
*      DSECT FOR REPORT HEADLINES                                               
*****************************************************                           
HEADD    DSECT                                                                  
HEADINV  DS    CL6                 INV #                                        
         DS    CL3                                                              
HEADPGM  DS    CL27                PROGRAM NAME                                 
         DS    CL3                                                              
HEADEFF  DS    CL17                EFF DATE                                     
         DS    CL3                                                              
HEADDPT  DS    CL4                 DAYPARTS                                     
         DS    CL1                                                              
HEADADJ  DS    CL4                 ADJUSTMENT                                   
         DS    CL3                                                              
HEADRES  DS    CL30                COMMENTS                                     
*                                                                               
*****************************************************                           
*      DSECT FOR BRDTAB                                                         
*****************************************************                           
BROADD   DSECT                                                                  
BRDQTR   DS    XL1                 QUARTER #                                    
BRDSTART DS    XL3                 START BROAD. MO (JULIAN)                     
BRDEND   DS    XL3                 END BROAD. MO (JULIAN)                       
BROADDLN EQU   *-BRDQTR                                                         
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REGAV19B  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
*&&DO                                                                           
*                                                                               
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STENDQTR)                             
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,BRDQTRST)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,STENDQTR+2)                         
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,BRDQTREN)                          
*                                                                               
         XC    WORK,WORK           GET FIRST AND LAST WEEK OF BRD YEAR          
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+4(2),=X'0C0F'                                               
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,STBRDYRJ)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,ENBRDYRJ)                          
*&&                                                                             
