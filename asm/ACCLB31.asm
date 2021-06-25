*          DATA SET ACCLB31    AT LEVEL 068 AS OF 08/16/00                      
*PHASE T62131A                                                                  
CLB31    TITLE '- BILL PROGRAM - GENERAL COLUMNS'                               
CLB31    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB31**,R8                                                    
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING MIXLST,LSMIXLST                                                  
         USING PRORATAD,LSPRATA                                                 
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     DISGEN              DISPLAY GENERAL COLUMN                       
         B     SETHEAD             SET HEADLINE                                 
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* GENERAL DISPLAY COLUMNS                                             *         
*                                                                     *         
* NTRY: R1 = ROUTINE NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
DISGEN   DS    0H                                                               
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,25                                                            
         SRL   R1,23                                                            
         LA    R1,ROUTAB-ROUTABL(R1)                                            
         LA    RF,ROUTABX                                                       
         CR    R1,RF                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,ROUDIS-ROUTABD(R1)                                            
         LA    RF,CLB31(RF)                                                     
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* DISK ADDRESS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISDA    GOTO1 VHEXOUT,BCDMCB,TLDA,FVIFLD,8,1,=C'MIX'                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TSAR INFO                                                           *         
***********************************************************************         
         SPACE 1                                                                
DISTSAR  XC    BCFULL,BCFULL                                                    
         MVC   BCFULL+2(L'TLNUM),TLNUM                                          
         CURED (B4,BCFULL),(4,FVIFLD),0                                         
         MVC   BCFULL+2(L'TLRLEN),TLRLEN                                        
         CURED (B4,BCFULL),(4,FVIFLD+5),0                                       
         XC    BCFULL,BCFULL                                                    
         MVC   BCFULL+3(L'TLKSES),TLKSES                                        
         CURED (B4,BCFULL),(4,FVIFLD+10),0                                      
         MVC   BCFULL+2(L'TLKSEQ),TLKSEQ                                        
         CURED (B4,BCFULL),(4,FVIFLD+15),0                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TSAR KEY                                                            *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   GOTO1 VHEXOUT,BCDMCB,TLKSRT,FVIFLD,36,1,=C'MIX'                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TSAR DATA                                                           *         
***********************************************************************         
         SPACE 1                                                                
DISDTA   GOTO1 VHEXOUT,BCDMCB,TLSTA,FVIFLD,34,1,=C'MIX'                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TSAR DATA FOR TRANSACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
DISBDTA  GOTO1 VHEXOUT,BCDMCB,TLOVR,FVIFLD,6,1,=C'MIX'                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TRANSACTION WORK-CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISTWC   MVC   FVIFLD(L'TRNKWORK),TRNANAL                                       
*                                                                               
         L     RF,BONTRYA          GIVE OVERLAY DISFIRST ROUTINE                
         ICM   RF,8,=AL1(O#DISCLM)                                              
         GOTO1 (RF),0              GOTO1 ODISCLM,0                              
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION CONTRA-ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
DISTCA   MVC   FVIFLD(L'TRNKULC),TRNKULC                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION REFERENCE#                                              *         
***********************************************************************         
         SPACE 1                                                                
DISTREF  MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION DATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISTDAT  GOTO1 VDATCON,BODMCB,(1,TRNKDATE),(17,FVIFLD)                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION MONTH-OF-ACTIVITY                                       *         
***********************************************************************         
         SPACE 1                                                                
DISTMOA  MVC   FVIFLD(L'TRNMOS),TRNMOS                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION BATCH REFERENCE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISTBRF  MVC   FVIFLD(L'TRNBREF),TRNBREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION BATCH TYPE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISTINP  XR    RF,RF                                                            
         IC    RF,TRNTYPE                                                       
         EDIT  (RF),(3,FVIFLD),WRK=BOWORK1,DUB=BODUB1                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION STATUS (DOESN'T DISPLAY MUCH ??)                        *         
***********************************************************************         
         SPACE 1                                                                
DISTSTA  MVC   FVIFLD(4),=C'....'                                               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION ORDER NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISTORD  LA    R3,TRNRFST                                                       
         USING FFNELD,R3                                                        
         SR    R0,R0                                                            
DISTO100 CLI   FFNEL,0                                                          
         BE    EXIT                                                             
         CLI   FFNEL,FFNELQ                                                     
         BE    DISTO200                                                         
         IC    R0,FFNLN                                                         
         AR    R3,R0                                                            
         B     DISTO100                                                         
DISTO200 MVC   FVIFLD(L'FFNONUM),FFNONUM                                        
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* COMMISSION RATE                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISTCMR  L     RE,AGOPBLK                                                       
         ZAP   BODUB1,GOAGYCOM-GOBLOCK(L'GOAGYCOM,RE)                           
         CURED BODUB1,(7,FVIFLD),4                                              
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* WRITE-OFF EXPENSE ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
DISTWOA  BAS   RE,GETWPTA                                                       
         BNE   EXIT                                                             
         LR    R3,RF                                                            
         USING PTAELD,R3                                                        
*                                                                               
         MVC   FVIFLD(L'PTAWEACT),PTAWEACT                                      
         MVC   BOHALF1,=C'SE'                                                   
         TM    TLXSTAT,TLXSULSI                                                 
         BZ    *+10                                                             
         MVC   BOHALF1,=C'SI'                                                   
         CLC   BOHALF1,PTAWEUNT                                                 
         BE    EXITY                                                            
         MVI   FVIFLD,C'*'                                                      
         MVC   FVIFLD+1(L'PTAWEXPA),PTAWEXPA                                    
         B     EXITY                                                            
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* OFFICE CODE (FOR WRITE-OFF POSTINGS)                                *         
***********************************************************************         
         SPACE 1                                                                
DISTOFF  CP    PP$AWOFF,BCPZERO                                                 
         BE    EXITY               NO WRITE-OFF PENDING                         
         L     RF,AIO5                                                          
         USING PTARECD,RF                                                       
         MVC   FVIFLD(L'TRNOFFC),PTARFST+(TRNOFFC-TRNELD)                       
         B     EXITY                                                            
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* PARAGRAPH CODE                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISTPAR  CP    PP$AALLO,BCPZERO    TEST ANYTHING PENDING                        
         BNE   *+14                                                             
         CP    PP$ACOMM,BCPZERO                                                 
         BE    EXIT                NOTHING PENDING-  NO PARA CODE               
         SR    R0,R0                                                            
         LA    R3,TRNRFST                                                       
         USING PTAELD,R3                                                        
DISTP02  CLI   PTAEL,0             FIND PTAEL FOR ALLOCATION PENDING            
         BE    EXIT                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   DISTP04                                                          
         CLI   PTATYPE,PTATRAL                                                  
         BNE   DISTP04                                                          
         TM    PTASTAT1,PTASPEND                                                
         BO    DISTP06                                                          
DISTP04  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     DISTP02                                                          
DISTP06  SR    RF,RF                                                            
         IC    RF,PTARCODE                                                      
         EDIT  (RF),(3,FVIFLD),WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT                
         B     EXITY                                                            
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* PTA RECORD DISK-ADDRESS                                             *         
***********************************************************************         
         SPACE 1                                                                
DISTPTA  BAS   RE,GETWPTA                                                       
         BNE   EXIT                                                             
         LR    R3,RF                                                            
         USING PTAELD,R3                                                        
*                                                                               
         LA    R4,IOKEY            READ PTA DIRECTORY RECORD                    
         USING PTARECD,R4                                                       
         XC    PTAKEY,PTAKEY                                                    
         MVI   PTAKTYP,PTAKTYPQ                                                 
         MVC   PTAKCPY,CUABIN                                                   
         MVC   PTAKJOB,BCJOBCOD                                                 
         MVC   PTAKSEQN,PTASEQN                                                 
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   DPTA10                                                           
         GOTO1 VHEXOUT,BODMCB,PTAKDA,FVIFLD,8,1,=C'MIX'                         
         B     EXIT                                                             
*                                                                               
DPTA10   MVC   FVIFLD(3),=C'N/F'   PTA RECORD NOT ON FILE                       
         GOTO1 VHEXOUT,BODMCB,PTASEQN,FVIFLD+4,4,1,=C'MIX'                      
         B     EXIT                                                             
         DROP  R3,R4                                                            
         SPACE 1                                                                
***********************************************************************         
* SUBREF (OTHNUM)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING OTHELD,R3                                                        
DISTSBR  LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
DISSBR02 IC    R0,OTHLN                                                         
         AR    R3,R0                                                            
         CLI   OTHEL,0                                                          
         BE    EXIT                                                             
         CLI   OTHEL,OTHELQ                                                     
         BNE   DISSBR02                                                         
         MVC   FVIFLD(L'OTHNUM),OTHNUM                                          
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* CONTRA ACCOUNT NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R3                                                       
DISTCTN  LA    R3,IOKEY            READ CONTRA HEADER RECORD                    
         MVC   CHDKEY,BCSPACES                                                  
         MVC   CHDKCULA,TRNKCULA                                                
         MVC   CHDKOFF,TRNKOFF                                                  
         MVC   CHDKCULC,TRNKCULC                                                
         XC    CHDKNULL,CHDKNULL                                                
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2                                                          
         LA    R3,CHDRFST                                                       
         USING CACELD,R3                                                        
         SR    RF,RF                                                            
         MVC   BOWORK1,BCSPACES                                                 
DISCTN02 CLI   CACEL,0                                                          
         BE    EXIT                                                             
         CLI   CACEL,CACELQ                                                     
         BE    DISCTN04                                                         
         CLI   CACEL,NAMELQ                                                     
         BE    DISCTN06                                                         
         IC    RF,CACLN                                                         
         AR    R3,RF                                                            
         B     DISCTN02                                                         
*                                                                               
DISCTN04 IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),CACNAME                                                
         B     EXIT                                                             
*                                                                               
         USING NAMELD,RF                                                        
DISCTN06 IC    RF,CACLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    EXIT                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXIT                                                             
         DROP  R3,RF                                                            
         SPACE 1                                                                
***********************************************************************         
* COMMISSIONABLE STATUS                                               *         
***********************************************************************         
         SPACE 1                                                                
DISTCST  LH    RE,=Y(UC8CMN-TWAD)                                               
         TM    TRNSTAT,TRNSNOCM                                                 
         BZ    *+8                                                              
         LH    RE,=Y(UC@NO-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         MVC   FVIFLD(1),0(RE)                                                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* UNIT                                                                *         
***********************************************************************         
         SPACE 1                                                                
DISUNT   LA    R3,TRNRFST                                                       
         XR    RF,RF                                                            
         USING UNPELD,R3                                                        
DUNT02   CLI   UNPEL,0                                                          
         BE    EXIT                                                             
         CLI   UNPEL,UNPELQ                                                     
         BE    *+12                                                             
         IC    RF,UNPLN                                                         
         BXH   R3,RF,DUNT02                                                     
*                                                                               
         CURED (P3,UNPUNIT),(5,FVIFLD),0,MINUS=YES                              
         CURED (P4,UNPRICE),(11,FVIFLD+5),CSCURBIL,MINUS=YES                    
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIND PENDING WRITE-OFF PTAEL - RETURN A(PTAEL) IN RF                *         
***********************************************************************         
         SPACE 1                                                                
GETWPTA  LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
         USING PTAELD,RF                                                        
GWPTA10  CLI   PTAEL,0                                                          
         BE    GWPTAN                                                           
         CLI   PTAEL,PTAELQ                                                     
         BE    GWPTA30                                                          
GWPTA20  IC    R0,PTALN                                                         
         AR    RF,R0                                                            
         B     GWPTA10                                                          
GWPTA30  CLI   PTATYPE,PTATWOF     TEST WRITE-OFF                               
         BNE   GWPTA20                                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BNO   GWPTA20                                                          
*                                                                               
GWPTAY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GWPTAN   LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL COLUMN SET HEADER ROUTINE                                   *         
*                                                                     *         
* NTRY: P1 = A(CLMTABD ENTRY)                                         *         
*       P2 = A(HEADLINE 1)                                            *         
*       P3 = A(HEADLINE 2)                                            *         
***********************************************************************         
         SPACE 1                                                                
SETHEAD  DS    0H                                                               
         LM    R2,R4,0(R1)         R2=CLMTAB,R3=HEAD1,R4=HEAD2                  
         USING CLMTABD,R2                                                       
         TM    CLMINDS2,CLMIPROR  CURRENCY HEADING                              
         BO    HCUR                                                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,CLMRTN                                                        
         SLL   RE,25                                                            
         SRL   RE,23                                                            
         LA    RE,ROUTAB-ROUTABL(RE)                                            
         LA    RF,ROUTABX                                                       
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,ROUHEAD-ROUTABD(RE)                                           
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         LA    RF,CLB31(RF)                                                     
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* INSERT CURRENCY CODE INTO HEADING                                   *         
***********************************************************************         
         SPACE 1                                                                
HCUR     CLC   CSCPYCUR,CSBILCUR   TEST AGENCY CURRENCY                         
         BE    EXIT                LEAVE HEADINGS AS THEY ARE                   
         SR    RF,RF               CENTRALISE CURRENCY CODE IN HEAD2            
         IC    RF,CLMHWDTH         COLUMN WIDTH                                 
         BCTR  RF,0                                                             
         LA    RE,0(RF,R4)                                                      
         CLI   0(R4),C' '          ENSURE R4=A(FIRST CHAR)                      
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
HCUR100  CLI   0(RE),C' '                                                       
         BH    HCUR200                                                          
         BCTR  RE,0                                                             
         BCT   RF,HCUR100                                                       
HCUR200  LR    RF,R4                                                            
         SR    RE,R4                                                            
         SH    RE,=Y(L'CSCPYCUR+1)                                              
         BNP   HCUR300                                                          
         SRDL  RE,32                                                            
         D     RE,=F'2'                                                         
         AR    RF,R4                                                            
         AR    RF,RE                                                            
HCUR300  MVI   0(RF),C'('                                                       
         MVC   1(L'CSBILCUR,RF),CSBILCUR                                        
         TM    CLMINDS2,CLMIAGYC                                                
         BZ    *+10                                                             
         MVC   1(L'CSCPYCUR,RF),CSCPYCUR                                        
         MVI   L'CSCPYCUR+1(RF),C')'                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISK ADDRESS                                                        *         
***********************************************************************         
         SPACE 1                                                                
HDA      MVC   0(L'DAH1,R3),DAH1                                                
         MVC   0(L'DAH2,R4),DAH2                                                
         B     EXIT                                                             
*        DC    C' DISK   '                                                      
DAH1     DC    X'40C489A292404040'                                              
*        DC    C'ADDRESS '                                                      
DAH2     DC    X'C184849985A2A240'                                              
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* TSAR DATA                                                           *         
***********************************************************************         
         SPACE 1                                                                
HTSAR    MVC   0(L'TSARH1,R3),TSARH1                                            
         MVC   0(L'TSARH2,R4),TSARH2                                            
         B     EXIT                                                             
*        DC    C'--------TSAR--------'                                          
TSARH1   DC    X'6060606060606060E3A281996060606060606060'                      
*        DC    C' NUM  LEN  SES  SEQ '                                          
TSARH2   DC    X'40D5A4944040D385954040E285A24040E2859840'                      
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* TSAR KEY                                                            *         
***********************************************************************         
         SPACE 1                                                                
HKEY     MVC   0(L'KEYH1,R3),KEYH1                                              
         MVC   0(L'KEYH2,R4),KEYH2                                              
         B     EXIT                                                             
*        DC    CL36'TSAR RECORD KEY'                                            
KEYH1    DC    X'E3A2819940998583969984409285A8'                                
KEYH2    DC    C'---------------'                                               
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* TSAR RECORD DATA                                                    *         
***********************************************************************         
         SPACE 1                                                                
HDTA     MVC   0(L'DTAH1,R3),DTAH1                                              
         MVC   0(L'DTAH2,R4),DTAH2                                              
         B     EXIT                                                             
*        DC    CL34'---------TSAR RECORD DATA---------'                         
DTAH1    DS    0CL34                                                            
         DC    C'---------'                                                     
         DC    X'E3A2819940998583969984408481A381'                              
         DC    C'---------'                                                     
*        DC    C'RECORD STATUS   RTACMASKS1        '                            
DTAH2    DS    0CL34                                                            
         DC    X'D9858396998440A2A381A3A4A2404040'                              
         DC    X'D9A3C183D481A292E2F14040404040404040'                          
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* PTA DISK ADDRESS                                                    *         
***********************************************************************         
         SPACE 1                                                                
HPTA     MVC   0(L'PTAH1,R3),PTAH1                                              
         MVC   0(L'PTAH2,R4),PTAH2                                              
         B     EXIT                                                             
*        DC    C'PTA DISK'                                                      
PTAH1    DC    X'D7E3C140C489A292'                                              
*        DC    C'ADDRESS '                                                      
PTAH2    DC    X'C184849985A2A240'                                              
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* TSAR TRANSACTION DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
HBDTA    MVC   0(L'BDTAH1,R3),BDTAH1                                            
         MVC   0(L'BDTAH2,R4),BDTAH2                                            
         B     EXIT                                                             
*        DC    C'-TSAR-'                                                        
BDTAH1   DC    X'60E3A2819960'                                                  
*        DC    C'STVAPE'                                                        
BDTAH2   DC    X'E2A3E581D785'                                                  
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* TRANSACTION UNIT DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
HUNT     XR    R0,R0               CHANGE "UNIT       PRICE"                    
         IC    R0,CLMHWDTH                "----------------"                    
HUNT02   MVI   0(R4),C'-'          TO     "UNIT       PRICE"                    
         CLI   0(R3),C' '                 "----       -----"                    
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,HUNT02                                                        
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
DMCB     EQU   BODMCB                                                           
O#DISCLM EQU   6                   OVERLAY DISCLM ROUTINE#                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
ROUTABD  DSECT                                                                  
ROUDIS   DS    XL2                 DISPLACEMENT TO DISPLAY ROUTINE              
ROUHEAD  DS    XL2                 DISPLACEMENT TO SETHEAD ROUTINE              
ROUTABL  EQU   4                                                                
         SPACE 1                                                                
CLB31    CSECT                                                                  
         PUSH  USING                                                            
         DROP                                                                   
         USING CLB31,0                                                          
ROUTAB   DS    0X                                                               
         DC    SL2(DISDA,HDA)      DISK ADDRESS                                 
         DC    SL2(DISTSAR,HTSAR)  TSAR INFO                                    
         DC    SL2(DISKEY,HKEY)    TSAR KEY                                     
         DC    SL2(DISTWC,0)       TRANSACTION WORK-CODE                        
         DC    SL2(DISTCA,0)       TRANSACTION CONTRA-ACCOUNT                   
         DC    SL2(DISTREF,0)      TRANSACTION REFERENCE #                      
         DC    SL2(DISTDAT,0)      TRANSACTION DATE                             
         DC    SL2(DISTMOA,0)      TRANSACTION MONTH OF ACTIVITY                
         DC    SL2(DISTBRF,0)      TRANSACTION BATCH REFERENCE                  
         DC    SL2(DISTINP,0)      TRANSACTION INPUT TYPE                       
         DC    SL2(DISTSTA,0)      TRANSACTION STATUS                           
         DC    SL2(DISTORD,0)      TRANSACTION ORDER NUMBER                     
         DC    SL2(DISTCMR,0)      TRANSACTION COMMISSION RATE                  
         DC    SL2(DISTWOA,0)      TRANSACTION WRITE-OFF ACCOUNT                
         DC    SL2(DISTOFF,0)      TRANSACTION OFFICE CODE                      
         DC    SL2(DISTPAR,0)      TRANSACTION PARAGRAPH CODE                   
         DC    SL2(DISTPTA,HPTA)   TRANSACTION PTA RECORD D/A                   
         DC    SL2(DISTSBR,0)      TRANSACTION SUB-REFERENCE                    
         DC    SL2(DISTCTN,0)      TRANSACTION CONTRA-ACCOUNT NAME              
         DC    SL2(DISTCST,0)      TRANSACTION COMMISSIONABLE STATUS            
         DC    SL2(DISDTA,HDTA)    TSAR DATA                                    
         DC    SL2(DISBDTA,HBDTA)  TSAR DATA FOR TRANSACTION                    
         DC    SL2(DISUNT,HUNT)    UNIT                                         
ROUTABN  EQU   (*-ROUTAB)/ROUTABL                                               
ROUTABX  DS    0X                                                               
         POP   USING                                                            
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
CLB31    CSECT                                                                  
         ORG   CLB31+(((*-CLB31)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068ACCLB31   08/16/00'                                      
         END                                                                    
