*          DATA SET REGAV16    AT LEVEL 061 AS OF 05/01/02                      
*PHASE T81316A,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81316 - REPPAK FILE MAINT - MASTER AVAIL MAINT'                
********************************************************************            
T81316   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81316,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81316+4096,R9                                                   
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
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*  MOVE PROFILE TO LOCAL WORKING STORAGE                                        
         LR    R3,RA                                                            
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                                                               
*        MVC   RERROR(2),=AL2(INVACT)                                           
*        LA    R2,CONACTH                                                       
*        B     ERREND                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VKEY     DS    0H                                                               
*              INIT WORK AREA                                                   
         MVI   KEYCHG,C'N'         INIT KEY SWITCH                              
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
*              SAVE THE REP                                                     
         MVC   REPHLD,AGENCY                                                    
*                                                                               
*              VALIDATE THE STATION                                             
         LA    R2,MINSSTAH                                                      
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    *+8                                                              
         MVI   KEYCHG,C'Y'         YES SET KEY TO TOP OF LIST                   
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
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
         MVC   CSTAT,STAHLD                                                     
         MVC   CCOSCRST,8(R2)                                                   
         SPACE 1                                                                
*                                                                               
*              RATE CODE                                                        
*                                                                               
         LA    R2,MINCODH                                                       
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    *+8                                                              
         MVI   KEYCHG,C'Y'         YES SET KEY TO TOP OF LIST                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             REQUIRED                                     
         BL    ERREND                                                           
         MVC   CODHLD,8(R2)                                                     
         OC    CODHLD,=4X'40'                                                   
*                                                                               
*              LENGTH                                                           
*                                                                               
****************************                                                    
*          DATA SET REGAV17    AT LEVEL 031 AS OF 03/08/96                      
*                                                                               
*  VALIDATE LENGTH                                                              
*                                                                               
         LA    R2,MINLENH                                                       
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    *+8                                                              
         MVI   KEYCHG,C'Y'         YES SET KEY TO TOP OF LIST                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
*                                                                               
         LR    RE,R2                                                            
         LA    RE,8(RE)            POINT TO FIELD                               
         ZIC   RF,5(R2)                                                         
         ZIC   R1,5(R2)                                                         
*--MUST BE NUMERIC                                                              
VK100    CLI   0(RE),C'0'                                                       
         BL    VK120                                                            
         CLI   0(RE),C'9'                                                       
         BH    VK120                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VK100                                                         
         B     VK140                                                            
*                                                                               
VK120    C     RF,=F'1'            IF NOT IN LAST POSITION ERROR                
         BNE   ERREND                                                           
*  CHECK FOR MINUTES/SECONDS INDICATOR                                          
         BCTR  R1,0                                                             
         CLI   0(RE),C'M'                                                       
         BNE   *+12                                                             
         OI    LENHLD,X'80'                                                     
         B     VK140                                                            
         CLI   0(RE),C'S'                                                       
         BNE   ERREND                                                           
*  CONVERT FIELD TO BINARY                                                      
VK140    BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,LENHLD+1                                                    
         SPACE 1                                                                
*                                                                               
*              QUARTER   QQ/YY EX. Q1/95                                        
*                                                                               
         LA    R2,MINQTRH                                                       
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BZ    *+8                                                              
         MVI   KEYCHG,C'Y'         YES SET KEY TO TOP OF LIST                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
*                                                                               
*--CHECK OUT THE QUARTER                                                        
         XC    WORK,WORK                                                        
         LA    RE,QTRTAB                                                        
         LA    RF,4                                                             
*                                                                               
*--MUST BE IN THE TABLE                                                         
VK200    CLC   8(2,R2),0(RE)                                                    
         BE    VK220               MUST BE NUMERIC                              
         LA    RE,7(RE)                                                         
         BCT   RF,VK200                                                         
         B     ERREND                                                           
VK220    MVC   QTRHLD,1(RE)                                                     
         NI    QTRHLD,X'0F'                                                     
         MVC   WORK+1(2),2(RE)     GET START DATE RANGE                         
         MVC   WORK+4(2),4(RE)     GET END DATE RANGE                           
         MVC   RATEQTR,6(RE)       GET QTR BIT                                  
*                                                                               
         CLI   10(R2),C'/'                                                      
         BNE   ERREND                                                           
*                                                                               
*--CHECK OUT THE YEAR                                                           
         MVC   HALF,11(R2)                                                      
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
*--MUST BE NUMERIC                                                              
VK300    CLI   0(RE),C'0'                                                       
         BL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         BH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VK300                                                         
*                                                                               
*--CONVERT YEAR TO BINARY                                                       
*                                                                               
         PACK  DUB(8),11(2,R2)                                                  
         CVB   R0,DUB                                                           
         STC   R0,YEARHLD                                                       
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
*--GET THE DATE RANGE                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(2,DTEHLD)                                  
         GOTO1 DATCON,DMCB,(3,WORK+3),(2,DTEHLD+2)                              
*                                                                               
         BAS   RE,CHKRATE          CHECK IF VALID RATE CODE                     
*                                                                               
         CLI   KEYCHG,C'Y'         HAS KEY CHANGED                              
         BNE   *+10                                                             
         XC    SAVEKEY,SAVEKEY     YES SET LIST TO TOP                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    VKXIT                                                            
         MVC   KEY(27),KEYSAVE     SET GENCON ERROR                             
*                                                                               
VKXIT    B     EXIT                                                             
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
QTRTAB   DC    CL2'Q1',XL5'0101031F80'                                          
         DC    CL2'Q2',XL5'0401061E40'                                          
         DC    CL2'Q3',XL5'0701091E20'                                          
         DC    CL2'Q4',XL5'0A010C1F10'                                          
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
*  USE REQUEST INFO TO SEE IF A VALID RATE RECORD EXISTS                        
*                                                                               
CHKRATE  NTR1                                                                   
         LA    R2,MINCODH                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         GOTO1 HIGH                                                             
         DROP  R6                                                               
*                                                                               
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R2,MINLENH                                                       
*  SET UP HELLO LOOKUP INFO                                                     
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BNE   ERREND                                                           
*  SEE IF QUARTER EXISTS ON THE RECORD                                          
         LA    R2,MINQTRH                                                       
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),RATEQTR      OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BNE   ERREND                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,MINSSTAH         STATION                                      
         MVC   8(4,R2),STAHLD                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,MINCODH          AVAIL CODE                                   
         MVC   8(4,R2),CODHLD                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,MINLENH          LENGTH                                       
         EDIT  (B1,LENHLD+1),(3,MINLEN)                                         
*                                                                               
         TM    LENHLD,X'80'        CHECK LENGTH IN MINUTES                      
         BZ    DK200                                                            
         OC    8(4,R2),SPACES                                                   
         LA    RE,8(R2)                                                         
         LA    RF,4                                                             
DK120    CLI   0(RE),X'40'                                                      
         BE    DK140                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,DK120                                                         
         DC    H'0'                                                             
DK140    MVI   0(RE),C'M'                                                       
*                                                                               
DK200    OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,MINQTR           QUARTER                                      
         MVI   0(R2),C'Q'                                                       
         EDIT  (B1,QTRHLD),(1,1(R2))                                            
         MVI   2(R2),C'/'                                                       
         EDIT  (B1,YEARHLD),(2,3(R2))                                           
         OI    MINQTRH+6,X'80'     TRANSMIT                                     
*                                                                               
DKXIT    CLI   CONACT,C'C'         IS ACTION CHANGE                             
         BNE   EXIT                                                             
         LA    R2,MINCST1H                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DREC     DS    0H                                                               
         NI    DMINBTS,X'7F'       TURN OFF READ FOR UPDATE                     
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         BAS   RE,CLRSCRN                                                       
         LA    R2,MININV1H                                                      
         ST    R2,THISLINE                                                      
         XCEF  KEYTAB,406                                                       
         MVI   KEYTAB,X'FF'                                                     
*                                                                               
         OC    SAVEKEY,SAVEKEY                                                  
         BZ    DR200                                                            
*                                                                               
         MVC   KEY(27),SAVEKEY     GET LAST RECORD                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                KEY MUST EXIST                               
         GOTO1 SEQ                 GET FIRST RECORD FOR SCREEN                  
         B     DR300                                                            
*                                                                               
DR200    XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
         GOTO1 HIGH                                                             
         B     DR300                                                            
*                                                                               
DR300    LA    R3,15                                                            
         LA    R4,KEYTAB                                                        
         B     DR360                                                            
*                                                                               
DR350    LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
DR360    CLC   KEY(17),KEYSAVE                                                  
         BNE   DR370                                                            
         CLI   RINVKSRC,0          MUST BE A HEADER                             
         BNE   DR350                                                            
         CLI   RINVKINV+3,0        ONLY NEW INVENTORIES ALLOWED                 
         BE    DR350                                                            
         B     DR390                                                            
DR370    XC    SAVEKEY,SAVEKEY     END OF INVOICES                              
         B     DREX                START POINTER AT THE TOP                     
*                                                                               
*  GET RECORD SEE IF DATE RANG QUALIFIES INVENTORY HEADER                       
*                                                                               
DR390    MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
         GOTO1 GETREC                                                           
* CHECK IF RECORD IN DATE RANGE                                                 
         CLC   RINVPEFF(2),DTEHLD+2       IS START AFTER END                    
         BH    DR350                                                            
         CLI   RINVPEFF+2,0               IS THERE A DATE RANGE                 
         BE    *+14                       NO DONT CHECK END DATE                
         CLC   RINVPEFF+2(2),DTEHLD       IS END BEFORE START                   
         BL    DR350                                                            
*                                                                               
*  MOVE KEY TO TABLE                                                            
*                                                                               
         MVC   SAVEKEY(27),KEY                                                  
         MVC   0(27,R4),KEY                                                     
         LA    R4,27(R4)                                                        
         MVI   0(R4),X'FF'                                                      
*                                                                               
         BAS   RE,DISPLAY                                                       
*                                                                               
         LA    R2,LINELEN(R2)      POINT TO NEXT LINE                           
         ST    R2,THISLINE                                                      
         BCT   R3,DR350                                                         
         B     DREX                                                             
*                                                                               
DREX     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              MOVE RECORD TO SCREEN                           *                
****************************************************************                
****************************************************************                
DISPLAY  NTR1                                                                   
         SPACE                                                                  
         L     R6,AIO1                                                          
         USING REINVREC,R6                                                      
*                                                                               
         L     R2,THISLINE         INVENTORY                                    
         MVC   8(4,R2),RINVKINV    INVENTORY                                    
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         L     R2,THISLINE         EFFECTIVE DATE                               
         LA    R2,EFFDH(R2)                                                     
         LA    R3,RINVPEFF         RECORD DATE                                  
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,8(R2))                                  
         CLI   2(R3),0                                                          
         BE    DISP280                                                          
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,17(R2))                                 
DISP280  OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
*  DAY/TIME                                                                     
         L     R2,THISLINE                                                      
         LA    R2,DAYTIM(R2)                                                    
*                                                                               
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,0(R2)             DAY                         
         LA    RE,17                                                            
*                                                                               
DISP320  CLI   0(R2),X'40'                                                      
         BNH   DISP340                                                          
         LA    R2,1(R2)                                                         
         BCT   RE,DISP320                                                       
         DC    H'0'                                                             
DISP340  MVI   0(R2),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R2))       TIME                        
         L     R2,THISLINE                                                      
         LA    R2,DAYTIMH(R2)                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R4                                                               
*                                                                               
*  PROGRAM                                                                      
         L     R2,THISLINE                                                      
         LA    R2,PROGH(R2)                                                     
*                                                                               
         USING RIPGELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         ZIC   R1,RIPGLEN                                                       
         S     R1,=F'2'                                                         
         C     R1,=F'24'           MAX OUTPUT SIZE                              
         BNH   *+8                                                              
         LA    R1,24                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RIPGNAME                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R4                                                               
*                                                                               
*  COST                                                                         
         L     R2,THISLINE                                                      
         LA    R2,COSTH(R2)                                                     
*                                                                               
         USING RIMAELEM,R4                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(4),CODHLD                                                    
         MVC   DUB+4(2),LENHLD                                                  
         MVC   DUB+6(1),YEARHLD                                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'06',(R6)),(9,ELEMKEY)               
         CLI   12(R1),0                                                         
         BNE   DISPEX                                                           
         L     R4,12(R1)                                                        
*--GET COST FROM THE CORRECT QUARTER                                            
         ZIC   RE,QTRHLD                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RF,RIMACST1                                                      
         AR    RF,RE                                                            
         ICM   RE,15,0(RF)                                                      
         SRDA  RE,32(0)                                                         
         D     RE,=F'100'                                                       
*                                                                               
         EDIT  (RF),(7,8(R2)),ALIGN=LEFT                                        
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R4                                                               
*                                                                               
DISPEX   B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,MININV1H         FIRST FIELD                                  
         LA    R3,MINTAGH          END OF SCREEN                                
*                                                                               
CLRSC20  CR    R2,R3                                                            
         BNL   CLRSCEX                                                          
*                                                                               
         ZIC   R1,0(R2)                                                         
         S     R1,=F'9'            GET LENGTH OF FIELD                          
         EX    R1,MVESPACE                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CLRSC60  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     CLRSC20                                                          
*                                                                               
CLRSCEX  B     EXIT                                                             
*                                                                               
MVESPACE MVC   8(0,R2),SPACES                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
         GOTO1 CHKLOCK                                                          
*                                                                               
         OI    DMINBTS,X'80'       TURN ON READ FOR UPDATE                      
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,MINCST1H                                                      
         LA    R4,KEYTAB                                                        
*                                                                               
*--SET UP ELEMENT                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING RIMAELEM,R5                                                      
         MVI   RIMACODE,X'06'                                                   
         MVI   RIMALEN,27                                                       
         MVC   RIMAREP,REPHLD                                                   
         MVC   RIMACDE,CODHLD                                                   
         MVC   RIMALNTH,LENHLD                                                  
         MVC   RIMAYEAR,YEARHLD                                                 
*                                                                               
VR100    MVI   ERROR,INVALID                                                    
*                                                                               
         CLI   0(R4),X'FF'         ARE WE AT THE END OF KEYTAB                  
         BE    VREX                YES EXIT                                     
*                                                                               
*--TEST NUMERIC                                                                 
         XC    COSTHLD,COSTHLD                                                  
         CLI   5(R2),0                                                          
         BE    VR450                                                            
         TM    4(R2),X'08'                                                      
         BZ    ERREND              MUST BE NUMERIC                              
         GOTO1 VPACK                                                            
         LR    RF,R0               MOVE COST IN RF                              
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         ST    RF,COSTHLD          SAVE THE COST                                
*                                                                               
*-READ THE RECORD                                                               
VR150    MVC   KEY(27),0(R4)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
*--LOOK UP THE ELEMENT SE IF IT EXISTS                                          
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'06',(R6)),(9,WORK+2)                
         CLI   12(R1),0                                                         
         BE    VR200               ELEMENT EXISTS CHANGE RECORD                 
         B     VR300               ADD NEW ELEMENT                              
*                                                                               
*--CHANGE THE ELEMENT                                                           
VR200    L     R5,12(R1)                                                        
         MVC   WORK,0(R5)                                                       
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'06',(R6)),(9,WORK+2)                
*                                                                               
         LA    R5,WORK                                                          
         MVI   WORK+1,27           MOVE LENGTH IN                               
*--POINT TO CORRECT QUARTER IN THE ELEMENT                                      
         ZIC   RE,QTRHLD                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RF,RIMACST1                                                      
         AR    RF,RE                                                            
         MVC   0(4,RF),COSTHLD     MOVE COST IN THE ELEMENT                     
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5),=C'ADD=CODE'              
         B     VR400                                                            
*                                                                               
*--BUILD NEW ELEMENT                                                            
VR300    LA    R5,WORK                                                          
*--POINT TO CORRECT QUARTER IN THE ELEMENT                                      
         ZIC   RE,QTRHLD                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RF,RIMACST1                                                      
         AR    RF,RE                                                            
         MVC   0(4,RF),COSTHLD     MOVE COST IN THE ELEMENT                     
         MVI   ERROR,AVLOVFLW      CHECK FOR RECORD OVERFLOW                    
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5),=C'ADD=CODE'              
         CLI   12(R1),5            CHECK RECORD TOO LONG                        
         BE    ERREND                                                           
         XC    RIMACST1(16),RIMACST1                                            
         B     VR400                                                            
*                                                                               
VR400    BAS   RE,MYFILWRT                                                      
VR450    LA    R4,27(R4)           NEXT KEYTAB ENTRY                            
         LA    R2,LINELEN(R2)      BUMP TO NEXT COST                            
         B     VR100                                                            
*                                                                               
VREX     B     DREC                                                             
         DROP  R6                                                               
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
*  BUMP TO NEXT SCREEN FIELD                                                    
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGAVD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* REGAVWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGAVFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVE3D                                                       
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENARTE                                                      
         EJECT                                                                  
       ++INCLUDE REGAVWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFDT  DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL1                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL15                TO BOOK CODES                                
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL60                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*****************************************************                           
*****************************************************                           
STAHLD   DS    CL5                 STATION HOLD AREA                            
ELEMKEY  DS    0CL9                                                             
REPHLD   DS    CL2                 MASTER AVAIL REP HOLD AREA                   
CODHLD   DS    CL4                 MASTER AVAIL CODE HOLD AREA                  
LENHLD   DS    CL2                 MASTER AVAIL LENGTH HOLD AREA                
YEARHLD  DS    CL1                 MASTER AVAIL YEAR HOLD AREA                  
QTRHLD   DS    CL1                 MASTER AVAIL QUARTER HOLD AREA               
DTEHLD   DS    CL4                 MASTER AVAIL DATE RANGE                      
COSTHLD  DS    F                   MASTER AVAIL COST HOLD AREA                  
*                                                                               
RATEQTR  DS    CL1                 QTR SETTING FOR RATE RECORD                  
KEYCHG   DS    CL1                 KEY CHANGE SET LIST TO TOP                   
*                                                                               
DATEDEB  DS    CL2                 DAY TO SUBTRACT FROM LAST INV                
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
KEYTAB   DS    CL406               KEY HOLD AREA 15KEYS 27BYTES EACH            
SAVEKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
THISLINE DS    A                   CURRENT LINE ADDRESS                         
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
*                                                                               
EFFDH    EQU   MINEFF1H-MININV1H                                                
EFFD     EQU   MINEFF1-MININV1H                                                 
DAYTIMH  EQU   MINDYT1H-MININV1H                                                
DAYTIM   EQU   MINDYT1-MININV1H                                                 
PROGH    EQU   MINPRG1H-MININV1H                                                
PROG     EQU   MINPRG1-MININV1H                                                 
COSTH    EQU   MINCST1H-MININV1H                                                
COST     EQU   MINCST1-MININV1H                                                 
LINELEN  EQU   MININV2H-MININV1H                                                
         EJECT                                                                  
*                                                                               
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061REGAV16   05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
