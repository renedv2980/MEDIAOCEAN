*          DATA SET ATTIMC     AT LEVEL 060 AS OF 03/03/86                      
*PHASE ATTIMC,*                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE ACSAVE                                                                 
ATTIMC   CSECT                                                                  
         TITLE 'ATTIMC - MCCANN AT&&T INTERFACE SYSTEM'                         
         PRINT NOGEN                                                            
         NBASE 0,ATTIMCAY,=V(ACSAVE),R8,R9,R7                                   
         L     R6,=V(SORTER)                                                    
         ST    R6,SORTER                                                        
         L     R6,=V(DATCON)                                                    
         ST    R6,DATCON                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R2,0(R1)                                                         
         CLC   2(7,R2),=C'NOTLIST'                                              
         BNE   LIST                                                             
         MVI   LISTSW,C'N'                                                      
         MVC   P(24),=C'TABLE LISTING SUPPRESSED'                               
LIST     GOTO1 DATCON,DMCB,(5,0),(1,SVRUNDT)                                    
         OPEN  (MATITB,(INPUT),MATDDSI,(INPUT))                                 
         MVC   TITLE+19(24),=C'AT&&T INTERFACE SYSTEM   '                       
         MVC   MID2(25),=C'* PHASE 1 - EDIT/FORMAT *'                           
         MVI   MID1,X'00'                                                       
         MVI   MID3,X'00'                                                       
         CLI   LISTSW,C'Y'                                                      
         BE    *+8                                                              
         PERF  PRT                                                              
         PERF  LOADCT              LOAD CLIENT/THEME TABLE                      
         PERF  LOADPS              LOAD PRODUCT SPLITTER TABLE                  
         PERF  LOADST              LOAD SUBTHEME SPLITTER TABLE                 
         PERF  LOADCC              LOAD CATEGORY CODE TABLE                     
         ZAP   LINE,=P'75'                                                      
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         MVC   MID4(34),=C'SYS ME CLI PRO JB/EST AMOS  ERRORS'                  
         CLI   C,C'/'              NO CARD MEANS BOMB                           
         BNE   *+6                                                              
BOMBC    DC    H'0'                                                             
         CLC   =C'AGENCY=',C       AGENCY= CARD MUST BE FIRST IN.               
         BNE   BOMBC                            ----                            
         MVC   AGY,C+7                                                          
         LA    R3,AGYTBL                                                        
AGY2     CLC   0(R3),X'FF'                                                      
         BNE   AGYOK                                                            
         ABEND 1,DUMP                                                           
         CLC   AGY,0(R3)                                                        
         BE    AGYOK                                                            
         LA    R3,25(R3)                                                        
         B     AGY2                                                             
AGYOK    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'TYPE=',C                                                      
         BE    SVTP                                                             
         ABEND 2                                                                
SVTP     MVC   TYPE,C                                                           
         LA    R2,MATINTER                                                      
         USING IHADCB,R2                                                        
         MVC   DCBDDNAM+4(2),AGY                                                
         MVC   TITLE(19),6(R3)                                                  
         MVC   AGENCY,2(R3)                                                     
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'/'                                                           
         BE    BOMBC                                                            
         CLC   C(12),=C'TYPERUN=DRAFT'                                          
         BE    DRFT                                                             
         MVI   LIVESW,C'Y'                                                      
         MVI   RUNTP,C'E'                                                       
         CLC   C(21),=C'TYPERUN=LIVE,ESTIMATE'                                  
         BE    GOTTYPE                                                          
         MVI   RUNTP,C'I'                                                       
         MVC   SORTCARD,INVSRC      CHANGE THE SORT SEQ FOR INVOICES            
         B     GOTTYPE                                                          
DRFT     MVI   RUNTP,C'E'                                                       
         CLC   C(22),=C'TYPERUN=DRAFT,ESTIMATE'                                 
         BE    GOTTYPE                                                          
         MVI   RUNTP,C'I'                                                       
         MVC   SORTCARD,INVSRC                                                  
         SPACE 3                                                                
GOTTYPE  CLC   TYPE+5(5),=C'MEDIA'                                              
         BE    GTT1                                                             
         B     GTT2                                                             
GTT1     MVI   MEDIA,C'M'                                                       
GTT2     CLI   C+79,C'A'                                                        
         BNE   OSORT                                                            
         MVI   ACCEPT,C'Y'                                                      
OSORT    GOTO1 SORTER,DMCB,SORTCARD,RECCARD,WORKREC                             
         PERF  CRHEAD              TO GET MONTCAP FILLED                        
         EJECT                                                                  
PHASE1   PERF  GETIN               GET INPUT                                    
         CLI   EOIN,C'E'                                                        
         BE    PHASE2                                                           
         USING DDSTD,R2                                                         
         PERF  VALCT               VALIDATE CLIENT AND GET THEME                
         BE    VALMD                                                            
         PERF  BADCT                                                            
VALMD    PERF  VALME               VALIDATE MEDIA AN GET CATEGORY CODE          
         BE    PSVAL                                                            
         PERF  BADME                                                            
PSVAL    PERF  VALPS               VALIDATE PRODUCT CODE                        
         BE    STVAL                                                            
         PERF  BADPS                                                            
STVAL    CLI   ERRSW,C'Y'          ANY ERRORS                                   
         BNE   PUTWK                                                            
ERS      MVI   ERRSW,C'N'                                                       
         MVI   ANYERRS,C'Y'        SUPPRESS PHASE 2                             
         B     PHASE1                                                           
PUTWK    CLC   =C'ATT',INCLI                                                    
         BNE   PUTWK2                                                           
         PERF  FORMATWK                                                         
         B     NOMORPR                                                          
PUTWK2   PERF  PRODSPLT            MUST ALLOW SPLITS AND MULTIPLE O/P           
         BNE   NOMORPR             NO MORE SPLITS                               
         PERF  FORMATWK            FORMAT A WORK RECORD.                        
         B     PUTWK                                                            
*                                  SINCE THE PRODUCT WAS VALIDATED              
*                                  BEFORE, A NO HERE MEANS NO MORE              
*                                  ENTRIES IN THE SPLITTER TABLE                
*                                  FOR THIS PRODUCT.                            
NOMORPR  MVI   PSCONT,C' '                                                      
         CLI   ERRSW,C'Y'                                                       
         BE    ERS                                                              
         B     PHASE1                                                           
         EJECT                                                                  
PHASE2   CLOSE MATITB                                                           
         MVC   P,SPACES                                                         
         PERF  PRT,T=3                                                          
         CLI   ANYERRS,C'Y'                                                     
         BNE   OPEN2                                                            
         CLI   ACCEPT,C'Y'                                                      
         BNE   SUPPRESS                                                         
         MVC   P(41),=C'* PHASE 2 WILL BE DONE EVEN WITH ERRORS *'              
         B     ACCEPT1                                                          
SUPPRESS MVC   P(40),=C'* PHASE 2 SUPPRESSED BECAUSE OF ERRORS *'               
         PERF  PRT                                                              
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE RC=16                                                            
         SPACE 3                                                                
OPEN2    MVC   P(35),=C'* NO ERRORS, PHASE 2 WILL BE DONE *'                    
ACCEPT1  PERF  PRT                                                              
         MVC   MID2(34),=C'* PHASE 2 - INTERFACE GENERATION *'                  
         ZAP   LINE,=P'75'                                                      
OPENOUT  OPEN  (MATINTER,(OUTPUT))                                              
         B     PH2SET                                                           
PH2SET   MVC   MID2+64(5),=C' LIVE'                                             
         MVI   SUB1,X'00'                                                       
         CLI   LIVESW,C'Y'                                                      
         BE    CEST                                                             
         MVC   MID2+64(5),=C'DRAFT'                                             
CEST     MVC   MID2+70(12),=C'ESTIMATE RUN'                                     
         CLI   RUNTP,C'E'                                                       
         BE    CRHD                                                             
         MVC   MID2+70(12),=C'INVOICE RUN '                                     
         SPACE 1                                                                
CRHD     MVC   MID2+83(15),=C' FOR PRODUCTION'                                  
         CLI   MEDIA,C'M'                                                       
         BNE   CRHD2                                                            
         MVC   MID2+83(15),=C' FOR MEDIA     '                                  
CRHD2    PERF  CRHEAD              CREATE HEADER RECORD                         
         SPACE 3                                                                
PH2RD    PERF  GETWK               GET RECORD FROM SORTER                       
         CLI   EOW,C'E'                                                         
         BE    EOJ                                                              
         PERF  FORMATL                                                          
         PERF  FORMATI                                                          
         B     PH2RD                                                            
         EJECT                                                                  
EOJ      PERF  WRAPRPT                                                          
         CLI   LIVESW,C'Y'                                                      
         BNE   XBASE2                                                           
         CLOSE MATINTER                                                         
XBASE2   XBASE RC=0                                                             
         EJECT                                                                  
*              CLIENT THEME TABLE LOAD                                          
LOADCT   NTR1                                                                   
LBR2     PERF  RTB                                                              
         CLC   0(9,R2),=C'CT $START'                                            
         BNE   LBR2                                                             
         LA    R3,PTTBL                                                         
LBR3     PERF  RTB                                                              
         CLC   0(7,R2),=C'CT $END'                                              
         BE    LBR9                                                             
         CLC   0(2,R2),=C'CT'                                                   
         BNE   LBR3                                                             
         MVC   0(1,R3),3(R2)       SYSTEM                                       
         MVC   1(3,R3),5(R2)       CLIENT                                       
         MVC   4(2,R3),9(R2)       ATT THEME                                    
         LA    R3,6(R3)                                                         
         B     LBR3                                                             
         SPACE 2                                                                
LBR9     MVI   0(R3),X'FF'                                                      
         ZAP   LINE,=P'75'                                                      
         XIT1                                                                   
         SPACE 3                                                                
RTB      NTR1                                                                   
         GET   MATITB                                                           
         LR    R2,R1                                                            
         CLI   LISTSW,C'Y'                                                      
         BNE   RTBX                                                             
         MVC   P(80),0(R2)                                                      
         PERF  PRT                                                              
RTBX     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              SUBTHEME SPLITTER LOAD                                           
LOADST   NTR1                                                                   
LBS2     PERF  RTB                                                              
         CLC   0(9,R2),=C'ST $START'                                            
         BNE   LBS2                                                             
         LA    R3,STTBL                                                         
         LA    R4,100                                                           
LBS2A    MVC   0(35,R3),SPACES                                                  
         LA    R3,35(R3)                                                        
         BCT   R4,LBS2A                                                         
         LA    R3,STTBL                                                         
LBS2B    PERF  RTB                                                              
         CLC   0(7,R2),=C'ST $END'                                              
         BE    LBS9                                                             
         CLC   0(2,R2),=C'ST'                                                   
         BNE   LBS2B                                                            
         ST    R2,SAVE2                                                         
         ST    R3,SAVE3                                                         
         MVC   0(1,R3),3(R2)       SYSTEM                                       
         MVC   1(3,R3),5(R2)       CLIENT                                       
         MVC   4(3,R3),9(R2)       PRODUCT                                      
         MVC   7(6,R3),13(R2)      JOB/EST                                      
         MVC   13(4,R3),20(R2)     MOS                                          
         PERF  LBS10,T=3                                                        
LBS8     L     R2,SAVE2                                                         
         L     R3,SAVE3                                                         
         LA    R3,35(R3)                                                        
         B     LBS2B                                                            
LBS9     MVI   0(R3),X'FF'                                                      
         ZAP   LINE,=P'75'                                                      
         B     XIT                                                              
         SPACE 2                                                                
LBS10    MVC   17(3,R3),25(R2)                                                  
         MVC   20(3,R3),29(R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,6(R3)                                                         
         BR    RE                                                               
         EJECT                                                                  
*              CATEGORY CODE TABLE LOAD                                         
LOADCC   NTR1                                                                   
LME2     PERF  RTB                                                              
         CLC   0(9,R2),=C'CC $START'                                            
         BNE   LME2                                                             
         LA    R3,CCTBL                                                         
LME3     PERF  RTB                                                              
         MVC   P(80),0(R2)                                                      
         CLC   0(7,R2),=C'CC $END'                                              
         BE    LME9                                                             
         CLC   0(2,R2),=C'CC'                                                   
         BNE   LME3                                                             
         MVC   0(2,R3),3(R2)                                                    
         MVC   2(3,R3),6(R2)                                                    
         LA    R3,5(R3)                                                         
         B     LME3                                                             
         SPACE 2                                                                
LME9     MVI   0(R3),X'FF'                                                      
         ZAP   LINE,=P'75'                                                      
         B     XIT                                                              
         SPACE 3                                                                
EOTB     MVC   P(17),=C'* EOF ON MASTER *'                                      
         PERF  PRT                                                              
         ABEND 1,DUMP                                                           
         EJECT                                                                  
LOADPS   NTR1                                                                   
LPS2     PERF  RTB                                                              
         CLC   0(9,R2),=C'PS $START'                                            
         BNE   LPS2                                                             
         LA    R3,PSTBL                                                         
         LA    R4,100                                                           
LPS3     MVC   0(20,R3),SPACES                                                  
         LA    R3,20(R3)                                                        
         BCT   R4,LPS3                                                          
         LA    R3,PSTBL                                                         
LPS4     PERF  RTB                                                              
         CLC   0(7,R2),=C'PS $END'                                              
         BE    LPS9                                                             
         CLC   0(2,R2),=C'PS'                                                   
         BNE   LPS4                                                             
         MVC   0(1,R3),3(R2)                                                    
         MVC   1(2,R3),5(R2)                                                    
         MVC   3(3,R3),8(R2)                                                    
         MVC   6(6,R3),12(R2)                                                   
         MVC   12(2,R3),19(R2)                                                  
         MVC   14(3,R3),22(R2)                                                  
         MVC   17(3,R3),26(R2)                                                  
         MVC   20(1,R3),30(R2)                                                  
         LA    R3,21(R3)                                                        
         B     LPS4                                                             
LPS9     MVI   0(R3),X'FF'                                                      
         ZAP   LINE,=P'75'                                                      
         B     XIT                                                              
         EJECT                                                                  
GETIN    NTR1                                                                   
GI1      GET   MATDDSI                                                          
         LR    R2,R1                                                            
         LR    R3,R1                                                            
         USING IERECD,R3                                                        
         PERF  SHUFFLE                                                          
         CLC   INCLI(6),=C'ACBACC'                                              
         BE    GI1                                                              
         CLC   INCLI(3),=C'ATC'                                                 
         BE    GI1                                                              
         CLI   RUNTP,C'I'                                                       
         BNE   CLIE                                                             
         CLI   ININVNO,C'0'        ONLY WITH INVOICE NUMBER                     
         BL    GI1                 ON BILLING RUN.                              
CLIE     CLI   RUNTP,C'E'                                                       
         BNE   NOFILT                                                           
         CLI   MEDIA,C'M'                                                       
         BE    NOFILT              NO FILTERING FOR MEDIA RUN                   
         CLI   INSYS,C'A'                                                       
         BNE   NOFILT                                                           
         LA    R1,MTB                                                           
COCAP    CLC   MONTCAP+6(3),0(R1)                                               
         BE    COFILT                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   COCAP                                                            
         DC    H'0'                                                             
COFILT   CLC   INFILT4,3(R1)                                                    
         BNE   GI1                                                              
NOFILT   CLI   MEDIA,C'M'          MEDIA RUN                                    
         BE    MED                                                              
         CLI   INSYS,C'A'                                                       
         BNE   GI1                 PROD RUN BYPASSES MEDIA                      
         CLI   INJBEST,C'X'        BYPASS 'X' JOBS FOR PRODUCTION               
         BE    GI1                                                              
         B     GOODMED                                                          
MED      CLI   INSYS,C'A'                                                       
         BE    GI1                 MEDIA RUN BYPASSES PRODUCTION                
GOODMED  CLC   =C'ATT',INCLI                                                    
         BNE   *+10                                                             
         MVC   INSUBT,=C'187'                                                   
         CLC   INOREST,SPACES                                                   
         BNE   CLCSN                                                            
         ZAP   INOREST,=P'0'                                                    
CLCSN    CLC   INSYS(2),=C'SN'     SPOT NETWORK IS NETPAK                       
         BE    NT                                                               
         CLI   INSYS,C'N'          IF NETPAK STRETCH EST NO TO 3                
         BNE   NOTNET                                                           
         PERF  SHIFTEST                                                         
NT       MVI   INSYS,C'N'                                                       
         MVC   INMEDIA+1(1),INEFILT   USE EST FILTER FOR MEDIA                  
         CLI   INEFILT,C' '                                                     
         BH    NNT2                   NO FILTER IS TEL                          
         MVI   INMEDIA+1,C'T'                                                   
         B     NNT2                                                             
NOTNET   MVC   INMEDIA+1(1),INMEDIA                                             
NNT2     MVC   INMEDIA(1),INSYS       OUT OF A 1 POS. CODE.                     
ZAPAM    CLI   ININVNO,C'0'                                                     
         BL    NOSBCD                                                           
         ZAP   SVAMT,INGROSS                                                    
         ZAP   SVAMT2,INGROSS                                                   
         CLI   INSYS,C'A'                                                       
         BE    NOSBCD                                                           
         SP    SVAMT,INDISC                                                     
         SP    SVAMT2,INDISC                                                    
NOSBCD   ZAP   SVOEST,INOREST                                                   
         MVI   SVTYPE,C'M'                                                      
         CLI   INSYS,C'A'                                                       
         BNE   GIREV                                                            
         CLI   INMEDIA+1,C'F'      PROD MEDIA F GETS MEDIA TYPE                 
         BE    GIREV                                                            
         MVI   SVTYPE,C'P'                                                      
GIREV    CLI   RUNTP,C'E'                                                       
         BNE   GISEEINV                                                         
*        CLI   INESTCD,C'R'         FOR ESTIMATE USE ONLY ORIGINAL              
*        BE    GI1                                                              
         CP    SVOEST,=P'0'         ONLY NON ZERO                               
         BE    GI1                                                              
*        CLC   INOEDT,SPACES                                                    
*        BE    GI1                                                              
GIX      XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
GISEEINV CLI   ININVNO,C'0'          ONLY WITH INV NO.                          
         BL    GI1                                                              
         B     GIX                                                              
         SPACE 2                                                                
EOFDDS   MVI   EOIN,C'E'                                                        
         B     XIT                                                              
         SPACE 3                                                                
SHIFTEST NTR1                                                                   
         CLC   INJBEST+1(2),=C'  '                                              
         BE    TWO                                                              
         CLI   INJBEST+2,C' '                                                   
         BE    ONE                                                              
         B     TRE                                                              
TWO      MVC   INJBEST+2(1),INJBEST                                             
         MVC   INJBEST(2),=C'00'                                                
         B     TRE                                                              
ONE      MVC   INJBEST+2(1),INJBEST+1                                           
         MVC   INJBEST+1(1),INJBEST                                             
         MVI   INJBEST,C'0'                                                     
TRE      MVC   INJBEST+3(3),=C'   '                                             
         B     XIT                                                              
         EJECT                                                                  
SHUFFLE  NTR1                                                                   
         CLC   IEREC,=C'IE'                                                     
         BE    SHUF                                                             
         CLC   IEREC,=C'EH'                                                     
         BNE   XIT                                                              
SHUF     LA    R3,NEWAREA                                                       
         MVC   0(250,R3),0(R2)                                                  
         MVC   0(250,R2),SPACES                                                 
         MVC   INSYS,IESYS                                                      
         MVC   INMEDIA(1),IEMEDIA                                               
         MVC   INCLI(12),IECLI                                                  
         MVC   INADVMO,IEMOS                                                    
         CLI   IEMOS,C'8'            ESTIMATES OUT OF NETPAK HAVE               
         BNL   ZPIOR                 BAD MOS BUT GOOD MOS IS IN                 
         MVC   INADVMO,IEESTNM+20    IEESTNM+20                                 
ZPIOR    ZAP   INOREST,=P'0'                                                    
         OC    IECEST,IECEST                                                    
         BZ    *+10                                                             
         ZAP   INOREST,IECEST                                                   
         GOTO1 DATCON,DMCB,(5,0),(1,INOEDT)                                     
         MVC   ININVDT(6),IEINVDT                                               
         MVC   ININVNO,IEINVNO                                                  
         CLI   IEINVNO,C'0'                                                     
         BL    NOBL                                                             
         ZAP   INBILLA,=P'0'                                                    
         ZAP   INGROSS,=P'0'                                                    
         ZAP   INNET,=P'0'                                                      
         ZAP   INDISC,=P'0'                                                     
         OC    IEPGROSS,IEPGROSS                                                
         BZ    *+16                                                             
         ZAP   INBILLA,IEPGROSS                                                 
         ZAP   INGROSS,IEPGROSS                                                 
         OC    IEPNET,IEPNET                                                    
         BZ    *+10                                                             
         ZAP   INNET,IEPNET                                                     
         OC    IEPCD,IEPCD                                                      
         BZ    *+10                                                             
         ZAP   INDISC,IEPCD                                                     
NOBL     MVC   INEFILT,IEFILTS                                                  
         MVC   INSUBT,IEESTNM                                                   
         MVC   INDUDT,=6C' '                                                    
         MVC   INFILT4,IEFILTS+3                                                
         B     XIT                                                              
NEWAREA  DS    CL250                                                            
         SPACE 2                                                                
PRT      NTR1                                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P,SPACES                                                         
         B     XIT                                                              
         EJECT                                                                  
VALCT    NTR1                                                                   
         MVI   BYPASS,C'N'                                                      
         LA    R3,PTTBL                                                         
VB2      CLI   0(R3),X'FF'                                                      
         BE    NO                                                               
VBNORM   CLC   0(1,R3),INSYS                                                    
         BNE   VB2A                                                             
         CLC   1(3,R3),INCLI                                                    
         BE    VB3                                                              
VB2A     LA    R3,6(R3)                                                         
         B     VB2                                                              
VB3      MVC   SVTHEME,4(R3)                                                    
         B     YES                                                              
         SPACE 2                                                                
         ANSR                                                                   
         SPACE 2                                                                
BADCT    NTR1                                                                   
         MVC   P+28(26),=C'CLIENT NOT ON THEME MASTER'                          
         PERF  PRERR                                                            
         MVI   ERRSW,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
VALME    NTR1                                                                   
         LA    R3,CCTBL                                                         
VM2      CLI   0(R3),X'FF'                                                      
         BE    NO                                                               
         CLC   0(2,R3),INMEDIA                                                  
         BE    VM3                                                              
         LA    R3,5(R3)                                                         
         B     VM2                                                              
VM3      MVC   SVMED,2(R3)                                                      
         B     YES                                                              
         SPACE 2                                                                
BADME    NTR1                                                                   
         MVC   P+28(25),=C'DDS MEDIA(S) NOT ON TABLE'                           
         PERF  PRERR                                                            
         MVI   ERRSW,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
VALST    NTR1                                                                   
         LA    R3,STTBL                                                         
VST1     CLC   INSYS,0(R3)                                                      
         BNE   VST2                                                             
         CLC   INCLI(12),1(R3)                                                  
         BNE   VST2                                                             
         CLI   13(R3),C'*'                                                      
         BE    YES                                                              
         CLC   INADVMO,13(R3)                                                   
         BNE   VST2                                                             
         B     YES                                                              
VST2     LA    R3,35(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   VST1                                                             
         B     NO                                                               
         EJECT                                                                  
VALPS    NTR1                                                                   
         CLC   =C'ATT',INCLI                                                    
         BE    VPSATT                                                           
         CLI   PSCONT,C'Y'           IS IT CONTINUED DIGGING FOR SPLITS         
         BE    VPS1                                                             
         LA    R3,PSTBL                                                         
         B     VPS2                                                             
VPS1     L     R3,SVR3               CONT DIG - RESTORE CURRENT ONE             
         B     VPS3                  AND THEN GO TO INCREASE TO NEXT            
VPS2     CLI   0(R3),X'FF'                                                      
         BE    NO                                                               
         CLI   0(R3),C'*'                                                       
         BE    VPS2A                                                            
         CLC   0(1,R3),INSYS                                                    
         BNE   VPS3                                                             
VPS2A    CLI   1(R3),C'*'                                                       
         BE    VPS2B                                                            
         CLC   1(2,R3),INMEDIA                                                  
         BNE   VPS3                                                             
VPS2B    CLC   3(3,R3),INPROD                                                   
         BNE   VPS3                                                             
         CLI   6(R3),C'*'                                                       
         BE    VPS2C                                                            
         CLC   6(6,R3),INJBEST                                                  
         BNE   VPS3                                                             
VPS2C    CLI   12(R3),C'*'                                                      
         BE    YESSV                                                            
         CLC   12(2,R3),INADVMO+2                                               
         BE    YESSV                                                            
VPS3     LA    R3,21(R3)                                                        
         B     VPS2                                                             
YESSV    ST    R3,SVR3                                                          
         B     YES                                                              
         SPACE 1                                                                
VPSATT   MVC   SVPROG,INPROD                                                    
         CLI   INSYS,C'N'                                                       
         BNE   YES                                                              
         MVC   SVPROG,=C'004'                                                   
         B     YES                                                              
         SPACE 1                                                                
BADPS    NTR1                                                                   
         MVC   P+28(22),=C'NOT ON PGM TRANS TABLE'                              
         PERF  PRERR                                                            
         MVI   ERRSW,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
PRERR    NTR1                                                                   
         CLI   ERRSW,C'Y'                                                       
         BE    PT1                                                              
         MVC   SV,P                                                             
         MVC   P,SPACES                                                         
         PERF  PRT                                                              
         MVC   P(100),SV                                                        
         MVC   P(28),SPACES                                                     
         MVC   P+7(3),INCLI                                                     
         MVC   P+1(1),INSYS                                                     
         MVC   P+11(3),INPROD                                                   
         MVC   P+4(2),INMEDIA                                                   
         MVC   P+15(6),INJBEST                                                  
         MVC   P+22(4),INADVMO                                                  
PT1      PERF  PRT                                                              
         B     XIT                                                              
         EJECT                                                                  
FORMATL  NTR1                                                                   
         LA    R5,INTREC                                                        
         USING ATTD,R5                                                          
FL0      NOP   FL1                                                              
         MVI   FL0+1,X'F0'                                                      
         MVC   MID4(40),SPACES                                                  
         MVC   MID2+40(9),MONTCAP                                               
         MVC   SUB2(26),=C'-----------DDS------------'                          
         MVC   SUB2+28(14),=C'-----AT&&T-----'                                  
         MVC   SUB3(26),=C'SYS ME CLI PRO JB/EST AMOS'                          
         MVC   SUB3+28(14),=C'THM PGM CAT TP'                                   
         CLI   RUNTP,C'E'                                                       
         BNE   INVHD                                                            
         MVC   SUB3+44(6),=C'EST NO'                                            
         MVC   SUB3+60(19),=C'AMOUNT     EST DATE'                              
         B     FL1                                                              
INVHD    MVC   SUB3+43(37),=C'INV NO   INV DATE F REV EST AMT   S/T'            
         MVC   SUB3+85(6),=C'AMOUNT'                                            
         MVC   SUB3+96(6),=C'DUE DT'                                            
FL1      MVC   P+1(1),DDSSYS                                                    
         MVC   P+4(2),DDSMED                                                    
         MVC   P+7(3),DDSCLI                                                    
         MVC   P+11(3),DDSPROD                                                  
         MVC   P+15(6),DDSJBEST                                                 
         MVC   P+22(4),DDSMOS                                                   
         MVC   P+29(2),ETHEME                                                   
         MVC   P+32(3),EPROG                                                    
         MVC   P+36(3),ECAT                                                     
         MVC   P+41(1),ETYPE                                                    
         CLI   RUNTP,C'E'                                                       
         BNE   FL2                                                              
         MVC   P+44(10),EESTNO                                                  
         MVC   P+70(9),EESTDATE                                                 
         PACK  SVAMT,EESTAMT                                                    
         EDIT  (P6,SVAMT),(12,P+56),2,COMMAS=YES,FLOAT=-                        
         AP    OESTT,SVAMT                                                      
         PERF  PRT,T=2                                                          
         B     XIT                                                              
FL2      MVC   P+43(7),IINVNO                                                   
         MVC   P+51(9),IINVDT                                                   
         MVC   P+61(1),IFINAL                                                   
         MVC   P+95(9),IDUEDT                                                   
         PACK  SVAMT,IREVEST                                                    
         EDIT  (P6,SVAMT),(12,P+62),2,COMMAS=YES,FLOAT=-                        
         AP    RESTT,SVAMT                                                      
         LA    R4,ISUB1                                                         
         LA    R6,3                                                             
         ZAP   SVAMT,=P'0'                                                      
FL3      CLI   0(R4),C' '                                                       
         BE    FL4                                                              
         MVC   P+77(3),0(R4)                                                    
         PACK  WORK2(6),3(14,R4)                                                
         EDIT  (P6,WORK2),(12,P+81),2,COMMAS=YES,FLOAT=-                        
         AP    SVAMT,WORK2(6)                                                   
         AP    INVT,WORK2(6)                                                    
         PERF  PRT                                                              
         AP    STCT,=P'1'                                                       
FL4      LA    R4,17(R4)                                                        
         BCT   R6,FL3                                                           
         CP    STCT,=P'2'                                                       
         BL    FL5                                                              
         MVC   P+77(3),=C'TOT'                                                  
         EDIT  (P6,SVAMT),(12,P+81),2,COMMAS=YES,FLOAT=-                        
         PERF  PRT,T=2                                                          
         ZAP   STCT,=P'0'                                                       
         B     XIT                                                              
FL5      PERF  PRT                                                              
         ZAP   STCT,=P'0'                                                       
         B     XIT                                                              
         EJECT                                                                  
PRODSPLT NTR1                                                                   
         MVI   SUFFIX,C' '                                                      
         PERF  VALPS                                                            
         BNE   NO               NO MORE ENTRIES, SAY NO                         
         MVI   PSCONT,C'Y'      SET CONTINUE FOR MORE ENTRIES                   
         L     R3,SVR3                                                          
         CLI   17(R3),C' '      BLANK=100%                                      
         BE    YESP                                                             
         CLC   17(3,R3),=C'100'                                                 
         BE    YESP                                                             
         PACK  WORK(3),17(3,R3)                                                 
         ZAP   WORK+3(9),SVAMT2                                                 
         MP    WORK+3(9),WORK(3)                                                
         DP    WORK+3(9),=P'100'                                                
         ZAP   SVAMT,WORK+3(7)                                                  
         ZAP   WORK+3(9),SVOEST                                                 
         MP    WORK+3(9),WORK(3)                                                
         DP    WORK+3(9),=P'100'                                                
         ZAP   INOREST,WORK+3(7)                                                
         B     YESP                                                             
YESP     MVC   SVPROG,14(R3)                                                    
         MVC   SUFFIX,20(R3)                                                    
         B     YES                                                              
         EJECT                                                                  
FORMATWK NTR1                                                                   
         LA    R5,WORKREC                                                       
         PERF  SPLITTER                                                         
         B     XIT                                                              
         SPACE 2                                                                
SPLITTER NTR1                                                                   
         LA    R3,STTBL                                                         
         LA    R4,3                                                             
         LA    R6,SVAMTS                                                        
SP0      ZAP   3(6,R6),=P'0'                                                    
         MVC   0(3,R6),SPACES                                                   
         LA    R6,9(R6)                                                         
         BCT   R4,SP0                                                           
SP1      CLI   0(R3),X'FF'                                                      
         BE    SP9A                                                             
         CLC   INSYS,0(R3)                                                      
         BNE   SP1A                                                             
         CLC   INCLI(12),1(R3)                                                  
         BNE   SP1A                                                             
         CLI   13(R3),C'*'                                                      
         BE    SP2                                                              
         CLC   INADVMO,13(R3)                                                   
         BE    SP2                                                              
SP1A     LA    R3,35(R3)                                                        
         B     SP1                                                              
SP2      LA    R4,3                                                             
         LA    R3,17(R3)                                                        
         LA    R6,SVAMTS                                                        
SP2A     CLI   0(R3),C' '                                                       
         BE    PERFFWK2                                                         
         PACK  WORK(2),3(3,R3)                                                  
         ZAP   OUTAMT,SVAMT                                                     
         MP    OUTAMT,WORK(2)                                                   
         DP    OUTAMT,=P'100'                                                   
         MVC   0(3,R6),0(R3)                                                    
         ZAP   3(6,R6),OUTAMT(6)                                                
         LA    R3,6(R3)                                                         
         LA    R6,9(R6)                                                         
         BCT   R4,SP2A                                                          
PERFFWK2 PERF  FORMATW2                                                         
SP9      B     XIT                                                              
SP9A     MVC   SVAMTS(3),INSUBT                                                 
         ZAP   SVAMTS+3(6),SVAMT                                                
         B     PERFFWK2                                                         
         SPACE 2                                                                
FORMATW2 NTR1                                                                   
         MVC   ESTIMATE,SPACES                                                  
         MVC   ECODE,RUNTP                                                      
         MVC   EAGY,AGENCY                                                      
         MVC   ETHEME,SVTHEME                                                   
         MVC   EPROG,SVPROG                                                     
         MVC   ECAT,SVMED                                                       
         MVC   ETYPE,SVTYPE                                                     
         MVC   SVMOS,INADVMO                                                    
         MVC   SVMOS+4(2),=C'01'                                                
         GOTO1 DATCON,DMCB,(0,SVMOS),(5,WORK)                                   
         MVC   MOSM,WORK                                                        
         MVC   DDSSYS,INSYS                                                     
         MVC   DDSMED,INMEDIA                                                   
         MVC   DDSCLI,INCLI                                                     
         MVC   DDSPROD,INPROD                                                   
         MVC   DDSJBEST,INJBEST                                                 
         MVC   DDSMOS,INADVMO                                                   
         CLI   ECODE,C'E'                                                       
         BNE   SP11                                                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(6),INJBEST                                                  
         PERF  ESUFFIX                                                          
         CLI   DDSSYS,C'A'         ONLY IF NOT PROD.                            
         BE    *+8                                                              
         PERF  ESUFFIX2            SUFFIX WITH MOS                              
         MVC   EESTNO,WORK                                                      
         UNPK  EESTAMT,INOREST                                                  
         MVC   EESTDATE,SPACES                                                  
SP10     GOTO1 DATCON,DMCB,(5,0),(5,WORK) USE TODAY'S DATE                      
SP10A    MVC   EESTDATE(3),WORK           FOR ESTIMATE DATE                     
         MVI   EESTDATE+3,C'/'            ALL THE TIME                          
         MVC   EESTDATE+4(5),WORK+3                                             
         B     SP12                                                             
SP11     MVC   WORK,SPACES                                                      
         MVC   WORK(6),ININVNO                                                  
         PERF  ESUFFIX                                                          
         MVC   IINVNO,WORK                                                      
         MVC   IINVNO+7(5),SPACES                                               
         CLI   INSYS,C'S'             SPOT & NET INVC DT IS FORMAT 0.           
         BE    SPTNET                                                           
         CLI   INSYS,C'N'                                                       
         BE    SPTNET                 PRINT & ACC ARE FORMAT 1                  
         GOTO1 DATCON,DMCB,(1,ININVDT),(5,WORK)                                 
         B     ACCPRT                                                           
SPTNET   GOTO1 DATCON,DMCB,(0,ININVDT),(5,WORK)                                 
ACCPRT   MVC   IINVDT(3),WORK                                                   
         MVI   IINVDT+3,C'/'                                                    
         MVC   IINVDT+4(5),WORK+3                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(6),INJBEST                                                  
         PERF  ESUFFIX                                                          
         CLI   DDSSYS,C'A'     IF NOT PROD. SUFFIX W/MOS                        
         BE    *+8                                                              
         PERF  ESUFFIX2                                                         
         MVC   IESTNO,WORK                                                      
         MVI   IFINAL,C' '                                                      
         CLI   INSYS,C'A'                                                       
         BNE   SP11A                                                            
         CLC   ININVDT,INJBCL                                                   
         BL    SP11A                                                            
         MVI   IFINAL,C'F'                                                      
SP11A    LA    R3,SVAMTS                                                        
         LA    R4,ISUB1                                                         
         LA    R6,3                                                             
SP11B    MVC   0(3,R4),0(R3)                                                    
         UNPK  3(14,R4),3(6,R3)                                                 
         LA    R3,9(R3)                                                         
         LA    R4,17(R4)                                                        
         BCT   R6,SP11B                                                         
         MVC   WORK,SPACES                                                      
CRE      MVC   IREVEST,=14C'0'                                                  
         PERF  DUEDT                                                            
         CLI   DDSSYS,C'A'                                                      
         BE    SP12                                                             
         CLI   INESTCD,C'R'     ONLY DO THE REVISED EST FOR MEDIA               
         BNE   SP12                                                             
         MVI   ICODE,C'R'                                                       
         UNPK  IREVEST,INOREST                                                  
SP12     GOTO1 SORTER,DMCB,=C'PUT',WORKREC                                      
         AP    SORTCT,=P'1'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ESUFFIX  NTR1                                                                   
         LA    R7,WORK                                                          
ES1      CLI   0(R7),C' '                                                       
         BE    PLOPIT                                                           
         LA    R7,1(R7)                                                         
         B     ES1                                                              
PLOPIT   MVC   0(1,R7),SUFFIX                                                   
         B     XIT                                                              
         SPACE 3                                                                
ESUFFIX2 NTR1                                                                   
         LA    R7,WORK                                                          
ES2      CLI   0(R7),C' '                                                       
         BE    PLOPIT2                                                          
         LA    R7,1(R7)                                                         
         B     ES2                                                              
PLOPIT2  MVC   0(3,R7),MOSM                                                     
         B     XIT                                                              
         SPACE 1                                                                
DUEDT    NTR1                                                                   
         LA    R1,DDTBL                                                         
CDD      CLI   0(R1),X'FF'                                                      
         BNE   CDD2                                                             
         ABEND 1,DUMP                                                           
CDD2     CLC   0(3,R1),IINVDT                                                   
         BE    DOYR                                                             
         LA    R1,7(R1)                                                         
         B     CDD                                                              
DOYR     PACK  WORK(2),IINVDT+7(2)                                              
         AP    WORK(2),6(1,R1)                                                  
         MVC   IDUEDT,=C'   /  /  '                                             
         UNPK  IDUEDT+7(2),WORK(2)                                              
         MVZ   IDUEDT+8(1),=X'F0'                                               
         MVC   IDUEDT+4(2),=C'15'                                               
         MVC   IDUEDT(3),3(R1)                                                  
         B     XIT                                                              
         SPACE 1                                                                
DDTBL    DC    C'JANFEB',PL1'0'   FR MO / TO MO / YR INCR.                      
         DC    C'FEBMAR',PL1'0'                                                 
         DC    C'MARAPR',PL1'0'                                                 
         DC    C'APRMAY',PL1'0'                                                 
         DC    C'MAYJUN',PL1'0'                                                 
         DC    C'JUNJUL',PL1'0'                                                 
         DC    C'JULAUG',PL1'0'                                                 
         DC    C'AUGSEP',PL1'0'                                                 
         DC    C'SEPOCT',PL1'0'                                                 
         DC    C'OCTNOV',PL1'0'                                                 
         DC    C'NOVDEC',PL1'0'                                                 
         DC    C'DECJAN',PL1'1'                                                 
         DC    X'FF'                                                            
MTB      DC    C'JAN1FEB2MAR3APR4MAY5JUN6JUL7AUG8SEP9OCTANOVBDECC'              
         DC    X'FF'                                                            
         EJECT                                                                  
GETWK    NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    EOWK                                                             
         MVC   INTREC,0(R6)                                                     
         B     XIT                                                              
EOWK     MVI   EOW,C'E'                                                         
         B     XIT                                                              
         EJECT                                                                  
FORMATI  NTR1                                                                   
         CLI   LIVESW,C'Y'                                                      
         BNE   XIT                                                              
         LA    R5,INTREC                                                        
         CLI   ECODE,C'E'                                                       
         BNE   INVC                                                             
         LA    R6,EESTAMT           WE MUST CHANGE THE FIELDS TO THE            
         PERF  EDITFLD              BIZZARE UNUSEABLE FORMAT ATT WANTS.         
         B     PUTI                                                             
INVC     LA    R6,ISUB1AMT                                                      
         PERF  EDITFLD                                                          
         LA    R6,ISUB2AMT                                                      
         PERF  EDITFLD                                                          
         LA    R6,ISUB3AMT                                                      
         PERF  EDITFLD                                                          
         LA    R6,IREVEST                                                       
         PERF  EDITFLD                                                          
PUTI     PUT   MATINTER,INTREC                                                  
         AP    TOTRECS,=P'1'                                                    
         B     XIT                                                              
         SPACE 2                                                                
EDITFLD  NTR1                                                                   
         PACK  SVAMT,0(14,R6)                                                   
         MVC   0(14,R6),SPACES                                                  
         CP    SVAMT,=P'0'                                                      
         BE    XIT                                                              
         EDIT  (P6,SVAMT),(14,(R6)),2,FLOAT=-                                   
         CLI   13(R6),C' '                                                      
         BE    XIT                                                              
         CLI   10(R6),C' '           THEY EVEN WANT A PRECEDING ZERO.           
         BNE   TRYMIN                                                           
         MVI   10(R6),C'0'                                                      
         B     XIT                                                              
TRYMIN   CLI   10(R6),C'-'                                                      
         BNE   XIT                                                              
         MVC   9(2,R6),=C'-0'                                                   
         B     XIT                                                              
         EJECT                                                                  
WRAPRPT  NTR1                                                                   
         PERF  PRT                                                              
         MVC   P+3(16),=C'TOTAL FOR REPORT'                                     
         CLI   RUNTP,C'E'                                                       
         BNE   TINV                                                             
         EDIT  (P8,OESTT),(14,P+49),2,COMMAS=YES,FLOAT=-                        
         B     TPT                                                              
TINV     EDIT  (P8,RESTT),(14,P+60),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,INVT),(14,P+79),2,COMMAS=YES,FLOAT=-                         
TPT      PERF  PRT,T=2                                                          
         MVC   P+3(30),=C'DRAFT RUN - NO RECORDS WRITTEN'                       
         CLI   LIVESW,C'Y'                                                      
         BNE   WRPRT                                                            
         AP    TOTRECS,=P'1'                                                    
         MVC   P+3(30),=C'INTERFACE RECORDS WRITTEN-    '                       
         EDIT  (P6,TOTRECS),(8,P+29),0,COMMAS=YES                               
         MVC   P+40(25),=C'(INCLUDING HEADER RECORD)'                           
WRPRT    PERF  PRT                                                              
         B     XIT                                                              
         EJECT                                                                  
CRHEAD   NTR1                                                                   
         LA    R5,INTREC                                                        
         CLI   PHASE,C'2'          MAY BE ONLY TO FILL MONTCAP                  
         BNE   CLIV                                                             
         AP    SORTCT,=P'1'                                                     
         UNPK  HRECORDS,SORTCT                                                  
         MVC   HTYPE,RUNTP                                                      
CLIV     LA    R3,C+16                                                          
CH0      CLC   =C',MONTH=',0(R3)                                                
         BE    CH0A                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNE   CH0                                                              
CH0A     LA    R4,MLIST                                                         
CH0B     CLC   7(3,R3),0(R4)                                                    
         BE    CH1                                                              
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   CH0B                                                             
         ABEND 1                    BAD OR MISSING MONTH PARM                   
CH1      MVC   HMONTH,7(R3)                                                     
         MVC   MONTCAP,1(R3)                                                    
         CLI   PHASE,C'2'                                                       
         BE    CHLIV                                                            
         MVI   PHASE,C'2'                                                       
         B     XIT                                                              
CHLIV    CLI   LIVESW,C'Y'                                                      
         BNE   XIT                                                              
         PUT   MATINTER,INTREC                                                  
         B     XIT                                                              
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(2,25,A),FORMAT=CH'                             
INVSRC   DC    CL80'SORT FIELDS=(2,13,A,36,12,A,15,6,A),FORMAT=CH'              
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=144'                                   
WORKREC  DC    CL144' '                                                         
DMCB     DS    6F                                                               
ACCEPT   DC    C'N'                                                             
MATITB   DCB   DSORG=PS,DDNAME=MATITB,LRECL=86,RECFM=FB,MACRF=GL,      X        
               EODAD=EOTB                                                       
MATDDSI  DCB   DSORG=PS,DDNAME=MATDDSI,LRECL=250,RECFM=FB,MACRF=GL,    X        
               BLKSIZE=2500,EODAD=EOFDDS                                        
MATINTER DCB   DSORG=PS,DDNAME=ATTIXX,LRECL=124,RECFM=FB,MACRF=PM,     X        
               BLKSIZE=1240                                                     
MLIST    DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',X'FF'                    
MEDIA    DC    C'P'                                                             
OUTAMT   DS    PL8'0'                                                           
INTREC   DC    CL144' '                                                         
TOTREC   DC    CL80' '                                                          
OESTT    DC    PL8'0'                                                           
RESTT    DC    PL8'0'                                                           
INVT     DC    PL8'0'                                                           
MONTCAP  DC    CL9' '                                                           
LIVESW   DC    C'N'                                                             
ERRSW    DC    C'N'                                                             
ANYERRS  DC    C'N'                                                             
INTFC    DC    C'N'                                                             
DUB      DS    D                                                                
WORK     DS    CL20                                                             
C        DS    CL80                                                             
EOIN     DC    C' '                                                             
SRECADD  DS    F                                                                
SAVE2    DS    F                                                                
SAVE3    DS    F                                                                
SVR3     DS    F                                                                
SVTHEME  DS    CL4                                                              
SVPROG   DS    CL3                                                              
SVMED    DS    CL3                                                              
SVEST    DS    CL5                                                              
SVREV    DS    CL1                                                              
SVTYPE   DS    CL1                                                              
SVRUNDT  DS    CL3                                                              
SVAMT    DC    PL6'0'                                                           
SVAMT2   DC    PL6'0'                                                           
SVOEST   DC    PL6'0'                                                           
SVMOS    DS    CL6                                                              
EOW      DC    CL1'N'                                                           
TOTRECS  DC    PL6'0'                                                           
LASTDIV  DC    CL2'  '                                                          
RUNTP    DS    C                                                                
OUTSUBT  DS    CL3                                                              
BTILL    DC    PL8'0'                                                           
TRECS    DC    PL6'0'                                                           
BYPASS   DC    C'N'                                                             
HSVSW    DC    C' '                                                             
SORTER   DS    A                                                                
DATCON   DS    A                                                                
SORTCT   DC    PL4'0'                                                           
SVAMTS   DS    0CL27                                                            
         DS    CL3,PL6                                                          
         DS    CL3,PL6                                                          
         DS    CL3,PL6                                                          
MOSM     DS    CL3                                                              
STCT     DC    PL1'0'                                                           
WORK2    DS    CL20                                                             
SV       DS    CL100                                                            
AGY      DS    CL2                                                              
AGENCY   DS    CL4                                                              
SUFFIX   DC    C' '                                                             
PSCONT   DC    C' '                                                             
LISTSW   DC    C'Y'                                                             
PHASE    DC    C'1'                                                             
TYPE     DS    CL10                                                             
         LTORG                                                                  
         EJECT                                                                  
*  AGYTBL FORMAT IS DDS AGY CODE(2) ATT AGY CODE(4) TITLE AGY(19)               
*                                                                               
AGYTBL   DC    C'MC',C'MCAN',CL19'MCCANN ERICKSON'                              
         DC    X'FF'                                                            
CCTBL    DS    50CL5                                                            
PTTBL    DS    100CL6                                                           
STTBL    DS    100CL35                                                          
PSTBL    DS    100CL21                                                          
       ++INCLUDE IERECD                                                         
       ++INCLUDE DDDPRINT                                                       
         DCBD  DSORG=PS,DEVD=TA                                                 
*                  DSECT TO COVER DDS INPUT RECORD                              
DDSTD    DSECT                                                                  
INREC    DS    0CL86                                                            
INSYS    DS    CL1                 SYSTEM S,P,A                                 
INMEDIA  DS    CL2                 MEDIA                                        
INCLI    DS    CL3                 CLIENT                                       
INPROD   DS    CL3                 PRODUCT                                      
INJBEST  DS    CL6                 JOB/ESTIMATE NO.                             
INADVMO  DS    CL4                 ADV SERVICE MONTH YYMM                       
INOREST  DS    PL6                 CURRENT ESTIMATE AMOUNT                      
         DS    CL12                                                             
INOEDT   DS    PL3                 CURRENT EST, DATE (DATE OF TP CREAT)         
ININVDT  DS    PL3                 INVOICE DATE                                 
INJBCL   DS    PL3                 JOB CLOSE DATE (PRODPAK ONLY)                
ININVNO  DS    CL6                 INVOICE NUMBER                               
INBILLA  DS    PL6                 ACTUAL BILLING AMT                           
INGROSS  DS    PL6                 GROSS AMT                                    
INNET    DS    PL6                 NET AMT                                      
INDISC   DS    PL6                 CASH DISCOUNT                                
INESTCD  DS    CL1                 R=ESTIMATE IS REVISION, BLANK=ORIG.          
INEFILT  DS    CL1                 ESTIMATE FILTER                              
INSUBT   DS    CL3                 SUBTHEME FROM 1ST 3 POS OF NAME              
INDUDT   DS    CL6                 DUE DATE (PAYABLE DATE)                      
INFILT4  DS    CL1                                                              
         DS    CL12                SPARE                                        
         EJECT                                                                  
*              DSECT TO COVER AT&T OUTPUT TAPE                                  
*                                                                               
ATTD     DSECT                                                                  
HEADER   DS    0CL144              -HEADER RECORD-                              
HTYPE    DS    CL1                 CODE 'E' OR 'I'                              
HMONTH   DS    CL3                 TAPE MONTH                                   
HRECORDS DS    CL4                 RECORDS                                      
         DS    CL136                                                            
         ORG   HEADER                                                           
ESTIMATE DS    0CL144              -ESTIMATE RECORD-                            
ECODE    DS    CL1                 CODE 'E'                                     
ETHEME   DS    CL2                 THEME                                        
EPROG    DS    CL3                 PROGRAM CODE                                 
ECAT     DS    CL3                 CATEGORY CODE                                
ETYPE    DS    CL1                 TYPE                                         
EAGY     DS    AL4                 AGENCY CODE 'MCAN'                           
EESTNO   DS    CL12                ESTIMATE NUMBER                              
EESTAMT  DS    CL14                ESTIMATE AMOUNT                              
EESTDATE DS    CL9                 EST DATE MMM/DD/YY                           
         DS    CL55                                                             
         ORG   ESTIMATE                                                         
INVOICE  DS    0CL144              -INVOICE RECORD-                             
ICODE    DS    CL1                 CODE 'I'                                     
ITHEME   DS    CL2                 THEME                                        
IPROG    DS    CL3                 PROGRAM CODE                                 
ICAT     DS    CL3                 CATEGORY CODE                                
ITYPE    DS    CL1                 TYPE CODE                                    
IAGY     DS    CL4                 AGENCY CODE                                  
IINVNO   DS    CL12                INVOICE NYMBER                               
IINVDT   DS    CL9                 INVOICE DATE MMM/DD/YY                       
IESTNO   DS    CL12                ESTIMATE NUMBER                              
IFINAL   DS    CL1                 FINAL INVOICE = F                            
ISUB1    DS    CL3                 SUB THEME 1                                  
ISUB1AMT DS    CL14                AMOUNT 1                                     
ISUB2    DS    CL3                 SUB THEME 2                                  
ISUB2AMT DS    CL14                AMOUNT 2                                     
ISUB3    DS    CL3                 SUB THEME 3                                  
ISUB3AMT DS    CL14                AMOUNT 3                                     
IDUEDT   DS    CL9                 DUE DATE                                     
IREVEST  DS    CL14                REVISED ESTIMATE AMOUNT                      
         DS    CL2                                                              
*                    END OF ATT RECORD                                          
DDSSYS   DS    CL1                 THE FOLLOWING STUFF IS DDS DATA              
DDSMED   DS    CL2                 FOR PRINTING, IT WILL NOT GO OUT             
DDSCLI   DS    CL3                 ON THE TAPE, THE ATT RCORD ENDS              
DDSPROD  DS    CL3                 WHERE INDICATED ABOVE.                       
DDSJBEST DS    CL6                                                              
DDSMOS   DS    CL4                                                              
         DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ATTIMC    03/03/86'                                      
         END                                                                    
