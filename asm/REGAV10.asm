*          DATA SET REGAV10    AT LEVEL 012 AS OF 04/29/09                      
*PHASE T81310C                                                                  
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81310 - REPPAK FILE MAINT - INV /ADD/CHA/DIS/DEL/ '            
********************************************************************            
* HISTORY OF CHANGES                                               *            
********************************************************************            
* FEB24/92 (BU ) --- PERMIT A NON-HIT ON INVENTORY DATE TO FIND    *            
*                    THE RECORD WITH THE APPROPRIATE EFFECTIVE     *            
*                    DATE RANGE.                                   *            
*                                                                  *            
* APR29/93 (SKU) --- CHANGE REC2+500 TO REC2+400 (WAS CLOBBERING   *            
*                    STORAGE AFTER END OF REC2)                    *            
*                                                                  *            
* MAR07/95 (BU ) --- FIX INV ADD TRAILER INFO DISAPPEARANCE        *            
*                                                                  *            
* JUN26/95 (BU ) --- FIX 'UT' DEMUP PROBLEM... HA!                 *            
*                                                                  *            
* APR29/09 (SMY) --- SUPPORT NEW INVENTORY KEY                     *            
*                                                                  *            
*                                                                  *            
*                    ***  END TOMBSTONE  ***                       *            
********************************************************************            
T81310   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81310,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81310+4096,R9                                                   
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
         MVC   REPAGNCY,AGENCY                                                  
         CLI   CPARREP,X'40'                                                    
         BNH   *+10                                                             
         MVC   REPAGNCY,CPARREP                                                 
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         CLI   MODE,RECDEL         DELETE                                       
         BE    DEL                                                              
         CLI   MODE,RECREST        AND RESTORE ARE INVALID                      
         BE    RESTORE                                                          
         BNE   EXIT                                                             
         MVC   RERROR(2),=AL2(INVACT)                                           
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
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
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
*              VALIDATE THE STATION                                             
         LA    R2,INVSSTAH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
         GOTO1 VALISTA                                                          
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
*              INVOICE NUMBER                                                   
*                                                                               
         XC    INVHLD,INVHLD                                                    
         MVI   ERROR,INVALID                                                    
         LA    R2,INVINVH                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VK100                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BE    VK140                                                            
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK140                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK140                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    VK140                                                            
         B     VK200                                                            
*  CHECK PROFILE SEE IF SELF DEFINE INVOICE SET                                 
VK100    TM    RMPPROFS,X'80'      ADD LOGIC                                    
         BO    VK140               BIT ON FIELD REQUIRED                        
         CLI   5(R2),0             NO INPUT ALLOWED SELF GENERATING NBR         
         BNE   ERREND                                                           
         B     VK440                                                            
*                                                                               
VK140    CLI   5(R2),0             REQUIRED LOGIC (CHA,DIS)                     
         BE    ERREND                                                           
         CLI   5(R2),4             MAX LENGTH IS 4                              
         BH    ERREND                                                           
         MVC   INVHLD(4),8(R2)                                                  
         OC    INVHLD(4),=4X'40'                                                
         B     VK440                                                            
*                                                                               
VK200    CLI   5(R2),0             OPTIONAL LOGIC (LIST)                        
         BE    VK440                                                            
         B     VK140                                                            
*                                                                               
*              EFFECTIVE DATE                                                   
*  OPTIONAL EXCEPT FOR ADD                                                      
VK440    LA    R2,INVEFFH                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VK480                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK480                                                            
         CLI   ACTNUM,ACTREST                                                   
         BE    VK480                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK560                                                            
*                                                                               
VK480    MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         XC    DTEHLD,DTEHLD                                                    
         XC    DTEHLD2(4),DTEHLD2                                               
*                                                                               
         LA    R2,INVEFFH          EDIT DATES                                   
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,WORK2),C',=,-'                              
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,1                                                         
         BNE   ERREND              THEY INPUT A ,                               
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK      START DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,DTEHLD)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2)                                 
         CLI   WORK2+1,0           NO END                                       
         BE    VK560                                                            
         MVC   DTEHLD2+2(2),WORK2+10                                            
         CLI   WORK2+1,1                                                        
         BNE   *+16                SHOULD BE A DATE                             
         TM    WORK2+3,X'80'       TEST FOR 1 POSITION NUMERIC                  
         BO    VK560                                                            
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2+2)                               
         CLC   DTEHLD2(2),DTEHLD2+2  START CANNOT BE > THEN END                 
         BH    ERREND                                                           
         SPACE                                                                  
*                                                                               
*                                                                               
VK560    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   CCONKSTA,STAHLD                                                  
         MVC   CCONINV,INVHLD                                                   
         MVC   CCONEFF,INVEFF                                                   
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPAGNCY                                                
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VKXIT                                                            
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKXIT                                                            
*                                                                               
         CLI   ACTNUM,ACTREST                                                   
         BE    VKXIT                                                            
*                                                                               
         LA    R2,INVSSTAH                                                      
         GOTO1 GETINV                                                           
*                                                                               
VKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
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
         LA    R2,INVSSTAH         STATION                                      
         MVC   8(4,R2),RINVKSTA                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,INVINVH          INVENTORY                                    
         MVC   8(4,R2),RINVKINV                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,INVEFF           EFFECTIVE DATE                               
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,0(R2))                               
         OI    INVEFFH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   CCONKSTA,RINVKSTA                                                
         MVC   CCONINV,RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,CCONEFF)                             
*                                                                               
DKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DREC     DS    0H                                                               
         CLC   CONACT(3),=CL3'SEL'                                              
         BNE   *+16                                                             
*  SAVE CURRENT LIST PARAMETERS                                                 
         MVC   SVLIST(188),LISTDIR                                              
         MVC   SVLIST+188(80),LSTONTWA                                          
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         BAS   RE,CLRSCRN                                                       
*                                                                               
*  EFFECTIVE DATE (KEY)                                                         
         LA    R2,INVEFF                                                        
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,0(R2))                               
         OI    INVEFFH+6,X'80'                                                  
*                                                                               
*  INV CODE                                                                     
         TM    RMPPROFS,X'80'      SELF DEFINED                                 
         BO    DREC40              DONT USE FIELD                               
         CLI   RINVKLEN,C'A'                                                    
         BL    DREC40                                                           
         CLI   RINVKLEN,C'Z'                                                    
         BH    DREC40                                                           
         MVC   INVICOD(1),RINVKLEN                                              
         OI    INVICODH+6,X'80'                                                 
*                                                                               
*  TRANSFER                                                                     
*DREC20  CLI   RINVPAUT,C'N'                                                    
*        BNE   DREC40                                                           
*        MVC   INVTRAN,=C'NT'      NO OVERNIGHT TRANSFER                        
*        OI    INVTRANH+6,X'80'                                                 
*                                                                               
*  DAYPART                                                                      
DREC40   MVC   INVSDPT,RINVDP                                                   
         OI    INVSDPTH+6,X'80'                                                 
*                                                                               
*  EFFECTIVE DATES                                                              
         LA    R2,INVEFF                                                        
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(5,0(R2))                               
         SPACE 1                                                                
         LA    R3,RINVPEFF+2                                                    
         OC    0(2,R3),0(R3)                                                    
         BZ    DREC60              NO END                                       
         LA    R2,8(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,1(R2))                                  
DREC60   OI    INVEFFH+6,X'80'                                                  
*                                                                               
*  FILTERS                                                                      
         MVC   INVSFLT,RINVPFLT                                                 
         OI    INVSFLTH+6,X'80'                                                 
*                                                                               
*  AVAIL DAY                                                                    
*        OC    RINVPADY,RINVPADY                                                
*        BZ    DREC80                                                           
*        XC    WORK,WORK                                                        
*        GOTO1 UNDAY,DMCB,RINVPADY,WORK                                         
*        MVC   INVADAY,WORK                                                     
*        OI    INVADAYH+6,X'80'                                                 
*                                                                               
*  AVAIL TIME                                                                   
* DREC80 MVC   INVATIM,SPACES                                                   
*        OC    RINVPATM,RINVPATM                                                
*        BZ    DREC100                                                          
*        GOTO1 UNTIME,DMCB,RINVPATM,INVATIM    AVAIL TIME                       
*        OC    RINVPATM+2(2),RINVPATM+2                                         
*        BNZ   DREC100                                                          
*        LA    R3,INVATIM                                                       
*        CLI   0(R3),C' '                                                       
*        BNH   *+12                                                             
*        LA    R3,1(R3)                                                         
*        B     *-12                                                             
*        MVC   0(2,R3),=C',B'                                                   
*DREC100 OI    INVATIMH+6,X'80'                                                 
*                                                                               
*  TRANSFER DEFAULTS                                                            
         LA    R2,INVTDEFH                                                      
         TM    RINVATD,X'80'                                                    
         BZ    *+14                                                             
         MVC   8(3,R2),=CL3'PAV'                                                
         B     DREC120                                                          
         TM    RINVATD,X'40'                                                    
         BZ    *+14                                                             
         MVC   8(3,R2),=CL3'TP '                                                
         B     DREC120                                                          
         TM    RINVATD,X'20'                                                    
         BZ    *+14                                                             
         MVC   8(3,R2),=CL3'TT '                                                
         B     DREC120                                                          
         MVC   8(3,R2),=CL3'PAV'                                                
DREC120  OI    INVTDEFH+6,X'80'                                                 
*                                                                               
*  GLOBAL PROTECTION SETTINGS                                                   
         LA    R2,INVTRAN                                                       
         LA    RE,3                                                             
         MVC   BYTE,RINVGPRO                                                    
DREC130  TM    BYTE,X'80'                                                       
         BZ    *+16                                                             
         MVI   0(R2),C'T'                                                       
         NI    BYTE,X'7F'                                                       
         B     DREC140                                                          
         TM    BYTE,X'40'                                                       
         BZ    *+16                                                             
         MVI   0(R2),C'C'                                                       
         NI    BYTE,X'BF'                                                       
         B     DREC140                                                          
         TM    BYTE,X'20'                                                       
         BZ    DREC150                                                          
         MVI   0(R2),C'D'                                                       
         NI    BYTE,X'DF'                                                       
DREC140  LA    R2,1(R2)                                                         
         BCT   RE,DREC130                                                       
DREC150  OI    INVTRANH+6,X'80'                                                 
*                                                                               
*                                                                               
*  DAY/TIME                                                                     
         LA    R2,INVSDAYH                                                      
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
DREC160  XC    WORK,WORK                                                        
         GOTO1 UNDAY,DMCB,RIDTDAY,WORK              DAY                         
         MVC   8(11,R2),WORK                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,8(R2))       TIME                        
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,RIDTLEN                                                       
         AR    R4,RE                                                            
         CLI   RIDTCODE,X'02'                                                   
         BNE   DREC180                                                          
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     DREC160                                                          
         DROP  R4                                                               
*                                                                               
*  PROGRAM                                                                      
DREC180  LA    R2,INVPROGH                                                      
         USING RIPGELEM,R4                                                      
         PRINT GEN                                                              
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',(R6)),0                         
         PRINT NOGEN                                                            
         CLI   12(R1),0                                                         
         BNE   DREC220                                                          
         L     R4,12(R1)                                                        
*                                                                               
DREC200  ZIC   R1,RIPGLEN                                                       
         S     R1,=F'3'                                                         
         LA    R3,RIPGNAME                                                      
         EX    R1,OUTFLD           MOVE TO SCREEN                               
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,RIPGLEN                                                       
         AR    R4,RE                                                            
         CLI   RIPGCODE,X'03'                                                   
         BNE   DREC220                                                          
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     DREC200                                                          
         DROP  R4                                                               
*                                                                               
*  AVAIL DAY/TIME                                                               
DREC220  LA    R2,INVADAYH                                                      
         USING RIAPELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'04',(R6)),0                         
         CLI   12(R1),0                                                         
         BNE   DRXIT                                                            
         L     R4,12(R1)                                                        
*                                                                               
DREC240  MVC   8(11,R2),RIADAY                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
         MVC   8(11,R2),RIATIME                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,RIAPLEN                                                       
         AR    R4,RE                                                            
         CLI   RIAPCODE,X'04'                                                   
         BNE   DRXIT                                                            
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     DREC240                                                          
         DROP  R4                                                               
*                                                                               
DRXIT    B     EXIT                                                             
*                                                                               
OUTFLD   MVC   8(0,R2),0(R3)                                                    
         DROP  R6                                                               
         SPACE 5                                                                
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,INVSDPTH         FIRST FIELD                                  
         LA    R3,INVLSTH          END OF SCREEN                                
*                                                                               
CLRSC20  CR    R2,R3                                                            
         BNL   CLRSCEX                                                          
         TM    1(R2),X'20'         CHECK IF PROTECTED                           
         BO    CLRSC60                                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               ANY INPUT                                    
         BZ    CLRSC60             NO, BYPASS                                   
         BCTR  R1,0                                                             
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
*              LIST ROUTINE IN OVERFLOW AREA                   *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
LIST     GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),RR=RELO    (LIST)                  
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DELETE RECORD ROUTINE                           *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DEL      DS    0H                                                               
         L     R4,AIO1                                                          
*                                                                               
         LA    R3,100                                                           
*                                                                               
         MVC   KEY(27),0(R4)       GET OLD KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
DEL100   DS    0H                                                               
*SMY*    CLC   KEY(24),KEYSAVE     CHECK UP TO RECORD TYPE                      
         CLC   KEY(RINVKRTP-RINVKEY),KEYSAVE   CHECK UP TO RECORD TYPE          
         BNE   DEL200                                                           
         OI    KEY+27,X'80'        DELETE KEY                                   
         BAS   RE,MYDIRWRT                                                      
         GOTO1 SEQ                 GET NEXT RECORD                              
         BCT   R3,DEL100                                                        
         DC    H'0'                OVER 100 RECORDS CHANGED TOO MANY            
*                                                                               
*  REMOVE THE PASSIVE POINTERS (R4 POINTS TO RECORD)                            
DEL200   GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              RESTORE RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
RESTORE  DS    0H                                                               
         L     R4,AIO1                                                          
*                                                                               
         LA    R3,100                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,INVSSTAH                                                      
         MVC   KEY(27),SAVEKEY     GET RECORD                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*SMY*    CLC   KEY(24),KEYSAVE     CHECK UP TO RECORD TYPE                      
         CLC   KEY(RINVKRTP-RINVKEY),KEYSAVE   CHECK UP TO RECORD TYPE          
         BNE   ERREND                                                           
*                                                                               
RST100   DS    0H                                                               
*SMY*    CLC   KEY(24),KEYSAVE     CHECK UP TO RECORD TYPE                      
         CLC   KEY(RINVKRTP-RINVKEY),KEYSAVE   CHECK UP TO RECORD TYPE          
         BNE   RST200                                                           
         NI    KEY+27,X'7F'        RESTORE KEY                                  
         BAS   RE,MYDIRWRT                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
         NI    DMINBTS,X'F7'                                                    
         BCT   R3,RST100                                                        
         DC    H'0'                OVER 100 RECORDS CHANGED TOO MANY            
*                                                                               
*  REMOVE THE OLD PASSIVE POINTERS (R4 POINTS TO OLD RECORD)                    
RST200   GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 RSTPT,DMCB,WORK2                                                 
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
         GOTO1 CHKLOCK                                                          
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPAGNCY                                                
         MVC   RINVKSTA,STAHLD                                                  
         SPACE 1                                                                
         XC    RINVPEL(256),RINVPEL                                             
         MVC   RINVPCOD(2),=XL2'0128'                                           
*  IF SELF ASSIGNED SET INDICATOR                                               
         NI    RINVSTAT,X'7F'                                                   
         TM    RMPPROFS,X'80'      ADD LOGIC                                    
         BZ    *+8                 BIT OFF COMPUTER ASSIGNED                    
         OI    RINVSTAT,X'80'      SET SELF ASSIGNED BIT                        
*  INV. CODE                                                                    
         MVI   RINVOINV+2,C'0'     SET DEFAULT VALUE                            
         TM    RMPPROFS,X'80'      SELF DEFINED                                 
         BO    VR40                DONT USE FIELD                               
         MVI   ERROR,INVALID                                                    
         LA    R2,INVICODH                                                      
         CLI   5(R2),0                                                          
         BE    VR40                                                             
         MVC   RINVKLEN,INVICOD                                                 
         CLI   RINVKLEN,C'A'                                                    
         BL    ERREND                                                           
         CLI   RINVKLEN,C'Z'                                                    
         BH    ERREND                                                           
         MVC   RINVOINV+2(1),RINVKLEN                                           
         SPACE 1                                                                
*  TRANSFER                                                                     
* VR20   LA    R2,INVTRANH                                                      
*        CLI   5(R2),0                                                          
*        BE    VR40                                                             
*        CLC   8(2,R2),=C'NT'                                                   
*        BNE   ERREND                                                           
*        MVI   RINVPAUT,C'N'       NO AUTOMATIC TRANSFER                        
*        SPACE 1                                                                
*  DAYPART                                                                      
*VR40     LA    R2,INVSDPTH                                                     
*         MVI   ERROR,INVALID                                                   
*                                                                               
*         XC    WORK,WORK                                                       
*         ZIC   R1,5(R2)                                                        
*         LTR   R1,R1                                                           
*         BZ    ERREND              NO INPUT ERROR                              
*         BCTR  R1,0                                                            
*         EX    R1,VARMOVE                                                      
*                                                                               
*         MVC   RINVDP,WORK         DAYPARTS TO RECORD                          
*         OC    RINVDP,=6X'40'      SPACE FILL                                  
*         SPACE 1                                                               
*         LA    R1,1(R1)                                                        
*         LA    R5,RINVDP                                                       
*         MVC   WORK(L'DPTBL+1),DPTBL                                           
*VR60     LA    RE,WORK                                                         
*         SPACE 1                                                               
*VR80     CLC   0(1,R5),0(RE)                                                   
*         BE    VR100                                                           
*         CLI   0(RE),X'FF'                                                     
*         BE    ERREND                                                          
*         LA    RE,1(RE)                                                        
*         B     VR80                                                            
*VR100    MVI   0(RE),X'FE'         ELIMINATE DUP DPTS-REMOVE FROM TABL         
*         LA    R5,1(R5)                                                        
*         BCT   R1,VR60                                                         
*         SPACE 1                                                               
         SPACE 1                                                                
*  DAYPART                                                                      
VR40     LA    R2,INVSDPTH                                                      
         MVI   ERROR,INVALID                                                    
         MVC   RINVDP,=6X'40'      SPACE FILL                                   
*                                                                               
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ERREND              NO INPUT ERROR                               
*                                                                               
*        GOTO1 VALDPTM,DMCB,(0,0(R2))                                           
*  CHECK DUPLICATES                                                             
         LA    R1,8(R2)            POINTS TO DAYPARTS                           
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
VR60     LA    R5,RINVDP           DAYPART FIELD                                
         LA    RE,6                MAXIMUM NUMBER OF DAYPARTS                   
*                                                                               
VR70     CLI   0(R5),X'40'         DAYPARTS TO RECORD                           
         BE    VR80                                                             
         CLC   0(1,R5),0(R1)       HAS ENTRY BEEN ADDED ALREADY                 
         BE    ERREND                                                           
         LA    R5,1(R5)                                                         
         BCT   RE,VR70                                                          
         DC    H'0'                IMPOSSIBLE                                   
VR80     MVC   0(1,R5),0(R1)       MOVE DAYPART TO RECORD                       
         LA    R1,1(R1)            GET NEXT DAYPART                             
         BCT   RF,VR60                                                          
         SPACE 1                                                                
*  EFFECTIVE DATES                                                              
         MVC   RINVKSTD(3),DTEHLD                                               
         MVC   RINVPEFF(4),DTEHLD2                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR140                                                            
         MVI   ERROR,INVALID                                                    
         LA    R2,INVEFFDH                                                      
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
         B     VR180                                                            
*  INPUT IN THIS FIELD WHEN CHANGE ACTION ONLY                                  
*                                                                               
*  FIRST MOVE DEFAULT DATE FROM ORIGINAL RECORD                                 
VR140    L     RE,AIO1                                                          
*SMY*    MVC   RINVKSTD(3),21(RE)                                               
         MVC   RINVKSTD(3),RINVKSTD-RINVKEY(RE)                                 
         MVC   RINVPEFF(4),41(RE)                                               
*                                                                               
         LA    R2,INVEFFDH                                                      
         CLI   5(R2),0                                                          
         BE    VR180                                                            
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,WORK2),C',=,-'                              
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,1                                                         
         BNE   ERREND              THEY INPUT A ,                               
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK      START DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RINVKSTD)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,RINVPEFF)                                
         CLI   WORK2+1,0           NO END                                       
         BE    VR180                                                            
         MVC   RINVPEFF+2(2),WORK2+10                                           
         CLI   WORK2+1,1                                                        
         BNE   *+16                SHOULD BE A DATE                             
         TM    WORK2+3,X'80'       TEST FOR 1 POSITION NUMERIC                  
         BO    VR180                                                            
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,RINVPEFF+2)                              
         CLC   RINVPEFF(2),RINVPEFF+2   START CANT BE > THEN END                
         BH    ERREND                                                           
         SPACE                                                                  
*  FILTERS                                                                      
VR180    MVC   RINVPFLT,SPACES                                                  
         MVC   WORK(6),SPACES                                                   
*                                                                               
         LA    R2,INVSFLTH              ADD THE FILTERS                         
         MVI   ERROR,INVALID                                                    
*                                                                               
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    VR260               NO INPUT ERROR                               
         BCTR  R1,0                                                             
         EX    R1,VARMOVE                                                       
*                                                                               
         MVC   RINVPFLT,WORK                                                    
         SPACE 1                                                                
         LA    R1,1(R1)                                                         
         LA    R5,RINVPFLT                                                      
VR200    LA    RE,BADFLT           CAN'T USE 1,2,3,4 AS FILTERS                 
*                         (USED TO BE FILTERS FOR FRINGE SUB-DAYPARTS)          
VR220    CLC   0(1,R5),0(RE)                                                    
         BE    ERREND                                                           
         CLI   0(RE),X'FF'                                                      
         BE    VR240                                                            
         LA    RE,1(RE)                                                         
         B     VR220                                                            
VR240    LA    R5,1(R5)                                                         
         BCT   R1,VR200                                                         
         SPACE 3                                                                
*  TRANSFER DEFAULTS                                                            
VR260    LA    R2,INVTDEFH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         OI    RINVATD,X'80'       SET 'PAV' AS DEFAULT                         
         B     VR280                                                            
*                                                                               
         CLC   8(3,R2),=CL3'PAV'                                                
         BNE   *+12                                                             
         OI    RINVATD,X'80'                                                    
         B     VR280                                                            
         CLC   8(2,R2),=CL2'TP'                                                 
         BNE   *+12                                                             
         OI    RINVATD,X'40'                                                    
         B     VR280                                                            
         CLC   8(2,R2),=CL2'TT'                                                 
         BNE   ERREND                                                           
         OI    RINVATD,X'20'                                                    
*                                                                               
*  GLOBAL PROTECTION SETTINGS                                                   
VR280    LA    R2,INVTRANH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    VR420                                                            
*                                                                               
         ZIC   RE,5(R2)                                                         
         LA    RF,INVTRAN                                                       
VR290    CLI   0(RF),C'T'                                                       
         BNE   *+12                                                             
         OI    RINVGPRO,X'80'                                                   
         B     VR295                                                            
         CLI   0(RF),C'C'                                                       
         BNE   *+12                                                             
         OI    RINVGPRO,X'40'                                                   
         B     VR295                                                            
         CLI   0(RF),C'D'                                                       
         BNE   ERREND                                                           
         OI    RINVGPRO,X'20'                                                   
VR295    LA    RF,1(RF)                                                         
         BCT   RE,VR290                                                         
         SPACE 3                                                                
*  AVAIL DAY                                                                    
* VR300  LA    R2,INVADAYH         EDIT THE AVAIL DAYS                          
*        XC    RINVPADY,RINVPADY                                                
*        CLI   5(R2),0                                                          
*        BE    VR340                                                            
*        MVI   ERROR,INVALID                                                    
*        XR    R5,R5                                                            
*        IC    R5,5(R2)            LENGTH OF EXPRESSION                         
*        GOTO1 DAYVAL,DMCB,((R5),8(R2)),RINVPADY,WORK                           
*        CLI   RINVPADY,0                                                       
*        BE    ERREND                                                           
*        SPACE                                                                  
*  AVAIL TIME                                                                   
* VR340  LA    R2,INVATIMH         EDIT THE AVAIL TIME                          
*        XC    RINVPATM,RINVPATM                                                
*        CLI   5(R2),0                                                          
*        BE    VR420                                                            
*        MVI   ERROR,INVALID                                                    
*        CLI   8(R2),C'N'          NONE AND                                     
*        BE    ERREND                                                           
*        CLI   8(R2),C'V'          VARIOUS ARE NOT VALID                        
*        BE    ERREND                                                           
*        SPACE 1                                                                
*        XR    R5,R5                                                            
*        IC    R5,5(R2)            LENGTH OF EXPRESSION                         
*        LA    R3,6(R5,R2)                                                      
*        CLC   0(2,R3),=C',B'                                                   
*        BNE   *+8                                                              
*        SH    R5,=H'2'                                                         
*        GOTO1 TIMVAL,DMCB,((R5),8(R2)),RINVPATM                                
*        CLI   DMCB,X'FF'                                                       
*        BE    ERREND                                                           
*        SPACE 1                                                                
*        CLC   0(2,R3),=C',B'                                                   
*        BNE   VR380                                                            
*        OC    RINVPATM+2(2),RINVPATM+2                                         
*        BNZ   ERREND                                                           
*        B     VR420                                                            
*        SPACE 1                                                                
* VR380  OC    RINVPATM+2(2),RINVPATM+2     IF NO END                           
*        BNZ   VR420                                                            
*        MVC   HALF,RINVPATM       ADD 30 MINUTES TO START                      
*        BAS   RE,TOMIN                                                         
*        LH    R5,HALF                                                          
*        LA    R3,30(R5)                                                        
*        LR    RF,R3                                                            
*        SPACE 1                                                                
*        CH    RF,=H'1440'         MAY PUSH IT PAST MIDNIGHT                    
*        BNH   *+8                                                              
*        SH    RF,=H'1440'                                                      
*        XR    RE,RE                                                            
*        SPACE 1                                                                
*        D     RE,=F'60'           GET MILITARY END FROM MINUTES                
*        MH    RF,=H'100'                                                       
*        AR    RF,RE                                                            
*        STH   RF,HALF                                                          
*        MVC   RINVPATM+2(2),HALF                                               
         SPACE 3                                                                
VR420    LA    R5,8                                                             
         LA    R2,INVSDAYH                                                      
         MVI   FIRSTSW,C'Y'                                                     
*  REMOVE CURENT DAY/TIME, PROG, AVAIL PROG ELEMENTS                            
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'02',AIO),0                          
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'03',AIO),0                          
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'04',AIO),0                          
*                                                                               
VR460    MVI   ERROR,INVALID                                                    
         BAS   RE,CHKDAYTM         CHECK DAY TIME IN SYNC                       
         CLI   DAYINP,C'Y'                                                      
         BE    VR480                                                            
         CLI   FIRSTSW,C'Y'        ONE DAY TIME REQUIRED                        
         BE    ERREND                                                           
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         B     VR500                                                            
*                                                                               
VR480    BAS   RE,VALDAY           DAY VALIDATION                               
         BAS   RE,NEXTFLD                                                       
         BAS   RE,VALTIME          TIME VALIDATION                              
         BAS   RE,NEXTFLD                                                       
VR500    BAS   RE,VALPROG          PROGRAM VALIDATION                           
         BAS   RE,NEXTFLD                                                       
         BAS   RE,VALADTM          AVAIL DAY/TIME                               
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         MVI   FIRSTSW,C'N'                                                     
         BCT   R5,VR460                                                         
         B     VREX                                                             
         DROP  R6                                                               
         SPACE 3                                                                
CHKDAYTM NTR1                                                                   
         MVI   ERROR,INVALID                                                    
*                                                                               
         LR    R3,R2               R2 POINTS TO DAY                             
         ZIC   RE,0(R3)                                                         
         AR    R3,RE               POINT R3 TO TIME FIELD                       
*  IF DAY INPUTTED TIME MUST ALSO BE INPUTTED                                   
         CLI   5(R2),0                                                          
         BE    CHKDY100                                                         
         CLI   5(R3),0                                                          
         BE    ERREND                                                           
         MVI   DAYINP,C'Y'         DAY INPUTTED                                 
         B     CHKDYEX                                                          
*  IF DAY NOT INPUTTED TIME CANNOT BE INPUTTED                                  
CHKDY100 CLI   5(R3),0                                                          
         BNE   ERREND                                                           
         MVI   DAYINP,C'N'         DAY NOT INPUTTED                             
*                                                                               
CHKDYEX  B     EXIT                                                             
         SPACE 3                                                                
VALDAY   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RIDTELEM,R6                                                      
         MVC   RIDTCODE(2),=XL2'0207'                                           
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VDY40                                                            
         CLI   FIRSTSW,C'Y'        MUST HAVE AT LEAST ONE DAY CODE              
         BE    ERREND                                                           
         B     VDYEX                                                            
*                                                                               
VDY40    XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),RIDTDAY,WORK                            
         CLI   RIDTDAY,0                                                        
         BE    ERREND                                                           
         SPACE 1                                                                
         CLI   FIRSTSW,C'Y'                                                     
         BNE   VDYEX                                                            
         L     R3,AIO                                                           
         USING REINVREC,R3                                                      
         GOTO1 =V(INVDAY),DMCB,((R5),8(R2)),RINVKDAY,WORK,DAYVAL,      X        
               RR=RELO                                                          
         MVC   RINVOINV+1(1),RINVKDAY                                           
         CLI   RINVKDAY,0                                                       
         BE    ERREND                                                           
*                                                                               
VDYEX    B     EXIT                                                             
         DROP  R3,R6                                                            
         SPACE 3                                                                
*--VALIDATE THE TIME FIELD (R2 POINTS TO TIME FIELD)                            
VALTIME  NTR1                                                                   
         USING RIDTELEM,R6                                                      
         LA    R6,WORK2                                                         
*                                                                               
         USING REINVREC,R4                                                      
         L     R4,AIO                                                           
*                                                                               
         GOTO1 ANY                                                              
VTM40    MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'N'          NONE                                         
         BE    ERREND                                                           
         CLI   8(R2),C'V'            VARIOUS ARE NOT VALID                      
         BE    ERREND                                                           
         SPACE 1                                                                
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         LA    R3,6(R5,R2)                                                      
         CLC   0(2,R3),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),RIDTTIME                                
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         SPACE 1                                                                
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=RELO    (GETQTR)                
         CLC   0(2,R3),=C',B'                                                   
         BNE   VTM80                                                            
         OC    RIDTTIME+2(2),RIDTTIME+2                                         
         BNZ   ERREND                                                           
         CLI   RINVKLEN,0                                                       
         BNE   VTMEX                                                            
         MVI   RINVKLEN,C'0'                                                    
         B     VTMEX                                                            
         SPACE 1                                                                
VTM80    CLC   RIDTTIME+2(2),=C'CC'                                             
         BNE   VTM100                                                           
         CLI   RINVKLEN,0                                                       
         BNE   VTMEX                                                            
         MVI   RINVKLEN,C'9'                                                    
         B     VTMEX                                                            
         SPACE 1                                                                
VTM100   MVC   HALF,RIDTTIME       START TIME TO MINUTES                        
         BAS   RE,TOMIN                                                         
         LH    R5,HALF             START MINUTE TO R5                           
         SPACE 1                                                                
         MVC   HALF,RIDTTIME+2     END TIME TO MINUTES                          
         BAS   RE,TOMIN                                                         
         LH    R3,HALF             END MINUTES                                  
         SPACE 1                                                                
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,30(R5)           IF NO END / ADD 30 TO START                  
         LR    RF,R3               END TIME MINUTES                             
         SPACE 1                                                                
         CH    RF,=H'1440'                                                      
         BNH   *+8                                                              
         SH    RF,=H'1440'         PAST MIDNIGHT                                
         XR    RE,RE                                                            
         SPACE 1                                                                
         D     RE,=F'60'           GET MILITARY END FROM MINUTES                
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         MVC   RIDTTIME+2(2),HALF                                               
         SPACE 1                                                                
         CLI   RINVKLEN,0                                                       
         BNE   VTMEX                                                            
         CR    R5,R3               START/END MINUTES                            
         BNH   *+8                                                              
         AH    R3,=H'1440'         ADD 24 X 60 TO END                           
         SPACE 1                                                                
         SR    R3,R5               END - START                                  
         LR    RF,R3                                                            
         XR    RE,RE                                                            
         D     RE,=F'30'           GET NUMBER 1/2 HOURS                         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RF,=H'1'            ADD 1 TO HALF HOURS                          
         STC   RF,RINVKLEN                                                      
         SPACE 1                                                                
         LA    R3,10               GET CODE FROM LENGTH TABLE                   
         LA    R5,LENGTH                                                        
         CLC   RINVKLEN,0(R5)                                                   
         BNH   *+14                                                             
         LA    R5,2(R5)                                                         
         BCT   R3,*-14                                                          
         DC    H'0'                                                             
         MVC   RINVKLEN,1(R5)                                                   
         MVC   RINVOINV+2(1),RINVKLEN                                           
*                                                                               
VTMEX    GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R6),=C'ADD=CODE'              
         B     EXIT                                                             
         DROP  R4,R6                                                            
         SPACE 3                                                                
VALPROG  NTR1                                                                   
         CLI   5(R2),0                                                          
         BNE   VPR50                                                            
         CLI   FIRSTSW,C'Y'                                                     
         BNE   VPREX                                                            
         MVI   ERROR,INVALID                                                    
         B     ERREND              THEY INPUT A ,                               
*                                                                               
VPR50    XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RIPGELEM,R6                                                      
*                                                                               
         MVI   RIPGCODE,X'03'                                                   
         SPACE 1                                                                
         ZIC   RE,5(R2)                                                         
         AH    RE,=H'2'                                                         
         STCM  RE,1,RIPGLEN                                                     
         MVC   RIPGNAME(27),8(R2)                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R6),=C'ADD=CODE'              
*                                                                               
VPREX    B     EXIT                                                             
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
         DROP  R6                                                               
         SPACE 3                                                                
VALADTM  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VAPEX                                                            
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RIAPELEM,R6                                                      
*                                                                               
         MVC   RIAPCODE(2),=X'0418'                                             
         SPACE 1                                                                
         MVC   RIADAY(11),8(R2)                                                 
         BAS   RE,NEXTFLD                                                       
         MVC   RIATIME(11),8(R2)                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R6),=C'ADD=CODE'              
*                                                                               
VAPEX    B     EXIT                                                             
         DROP  R6                                                               
         SPACE 3                                                                
*              BYTE 1  = NUMBER HALF HOURS, BYTE 2 CODE                         
*                                                                               
LENGTH   DC    AL1(01),C'0'        UP TO 1/2 HOUR                               
         DC    AL1(02),C'1'        FROM MORE THAN 1/2  TO     1 HOUR            
         DC    AL1(03),C'2'                         1     1 1/2                 
         DC    AL1(04),C'3'                     1 1/2         2                 
         DC    AL1(05),C'4'                         2     2 1/2                 
         DC    AL1(06),C'5'                     2 1/2         3                 
         DC    AL1(08),C'6'                         3         4                 
         DC    AL1(12),C'7'                         4         6                 
         DC    AL1(16),C'8'                         6         8                 
         DC    AL1(99),C'9'        OVER 8 HOURS                                 
         SPACE 3                                                                
VREX     L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   DATEDEB,RINVPEFF+2  SAVE END DATE                                
         CLI   RINVPEFF+2,0                                                     
         BNE   *+8                                                              
         MVI   RINVPEFF+3,0                                                     
*                                                                               
         TM    RMPPROFS,X'80'      USER GENERATED INVENTORY NUMBER              
         BZ    *+16                NO USE CALCULATED INV#                       
         MVC   RINVKINV(4),INVINV  MOVE IN USER NUMBER                          
         OC    RINVKINV(4),=4X'40'                                              
         MVC   CCONINV,RINVKINV    FOR PFM TRANSFER                             
         MVC   INVINV(4),RINVKQTR  DISPLAY INVENTORY NUMBER                     
         OI    INVINVH+6,X'80'                                                  
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),RR=RELO    (ACTIVITY)              
         BAS   RE,MAINTREC                                                      
         BAS   RE,ENDOLD                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 1                                                                
DPTBL    DC    C'MDERATLWKNPVSJOXYUZ'                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
BADFLT   DC    C'1234'                                                          
         DC    X'FF'                                                            
         SPACE 4                                                                
         SPACE 3                                                                
TOMIN    NTR1                                                                   
         XR    RE,RE                                                            
         LH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         MH    RF,=H'60'                                                        
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO DO FILE AND DIRECTORY MAINTENENCE                              
*                                                                               
MAINTREC NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   MNT200                                                           
*  CLEAR KEY PORTION OF AIO1                                                    
         L     R4,AIO1                                                          
         XC    0(27,R4),0(R4)                                                   
*  CHECK OVERLAPPING DATES                                                      
         L     R6,AIO2                                                          
         MVC   KEY(27),0(R6)                                                    
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO1                                                          
         USING REINVREC,R4                                                      
*                                                                               
         LA    R2,INVSSTAH                                                      
*                                                                               
         GOTO1 GETINV                                                           
         CLC   RERROR,=AL2(NOTFOUND)                                            
         BE    MNT40                                                            
*                                                                               
         MVI   ERROR,OVERLAP                                                    
         CLC   0(21,R4),0(R6)      DOES RETURNED RECORD QUALIFY                 
         BNE   MNT40               NO EXIT                                      
*  IF PRIOR RECORD DOES NOT HAVE AN AND DATE THEN NO RECORDS CAN BE             
*  ADDED AFTER THAT DATE. UNLESS A DEBIT NUMBER IS INPUTTED WITH                
*  THE CURRENT RECORD.                                                          
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BNZ   MNT40                                                            
*  IF CURRENT RECORD HAS AN END DATE NO OVERLAP ERROR SHOUL BE SET              
         OC    DATEDEB,DATEDEB                                                  
         BZ    ERREND                                                           
*        CLI   DATEDEB,0           IS IT A DEBIT NUMBER OR DATE                 
*        BE    MNT40               DEBIT NUMBER OK                              
*        B     ERREND                                                           
*                                                                               
*  MNT30    CLC   RINVPEFF+2,41(R6)                                             
*           BNL   ERREND                                                        
*                                                                               
MNT40    MVC   AIO,AIO2            RESET AIO AREA                               
*  CHECK DUPLICATE KEY                                                          
         MVI   ERROR,DUPLICAT                                                   
         CLC   0(27,R6),0(R4)                                                   
         BE    ERREND                                                           
*                                                                               
*                                                                               
*  ADD ACTION WRITE THE FILE AND ADD BOTH PRIMARY AND SECONDARY KEYS            
*                                                                               
         L     R6,AIO                                                           
*                                                                               
*        MVC   KEY(27),0(R6)                                                    
*        GOTO1 HIGH                                                             
*        CLC   KEY(27),KEYSAVE     CHECK FOR DUPLICATE KEY                      
*        BNE   MNT80                                                            
*        LA    R2,INVINVH                                                       
*        MVI   ERROR,DUPLICAT                                                   
*        B     ERREND                                                           
MNT80    BAS   RE,MYFILADD                                                      
*  ADD THE PASSIVE POINTERS                                                     
         GOTO1 INVPTR,DMCB,0(R6),WORK2                                          
         GOTO1 NWPT,DMCB,WORK2                                                  
         B     EXIT                                                             
*                                                                               
*  CHANGE CHECKS TO SEE IF KEY CHANGE                                           
*                                                                               
MNT200   CLI   ACTNUM,ACTCHA                                                    
         BE    MNT220                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   EXIT                                                             
MNT220   L     R4,AIO1             ORIGINAL RECORD ***************              
         L     R6,AIO2             NEW RECORD ********************              
* CHECK FOR KEY CHANGE                                                          
         CLC   0(27,R4),0(R6)                                                   
         BNE   MNT300                                                           
* NO KEY CHANGE JUST WRITE THE RECORD OUT                                       
MNT260   BAS   RE,MYFILWRT                                                      
         BAS   RE,CHADYTM          CHANGE DAY TIMES ON TRACK RECS               
* CHECK FOR CHANGE IN DAYPART FIELD/OR INVENTORY # CHANGE                       
         CLC   RINVDP,RINVDP-RINVREC(R6)        DP CHANGE                       
         BNE   MNT600                                                           
         CLC   RINVOINV,RINVOINV-RINVREC(R6)    INV CHANGE                      
         BE    EXIT                                                             
         B     MNT600              CREATE NEW SECONDARY KEYS                    
* KEY CHANGE DELETE OLD KEYS CRTEATE NEW KEYS                                   
MNT300   DS    0H                                                               
*SMY*    LA    RE,21                                                            
         LA    RE,RINVKSTD-RINVKEY                                              
*  WAS EFFECTIVE DATE CHANGED                                                   
*SMY*    CLC   21(3,R4),21(R6)                                                  
         CLC   RINVKSTD-RINVKEY(3,R4),RINVKSTD-RINVKEY(R6)                      
         BE    *+8                                                              
*SMY*    LA    RE,24                                                            
         LA    RE,RINVKSPR-RINVKEY                                              
         STC   RE,CHNGLEN                                                       
*                                                                               
         MVC   KEY(27),0(R6)       MOVE NEW INVENTORY SCREEN                    
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BNE   MNT360                                                           
         LA    R2,INVINVH                                                       
         MVI   ERROR,DUPLICAT                                                   
         B     ERREND                                                           
*                                                                               
MNT360   MVC   SAVEKEY,KEYSAVE     SAVE NEW KEY                                 
         BAS   RE,FLADD            WRITE RECORD OUT                             
         BAS   RE,CHAOLD           UPDATE THE CHANGED RECORDS                   
         BAS   RE,CHADYTM          CHANGE DAY TIMES ON TRACK RECS               
*                                                                               
*  DELETE ALL THE KEYS WITH THE OLD INVENTORY NUMBER/EFFECTIVE DATE             
*                                                                               
*        L     R2,AIO3                                                          
*        LA    R3,100                                                           
*                                                                               
*        MVC   KEY(27),0(R4)       GET OLD KEY                                  
*        GOTO1 HIGH                                                             
*        MVC   BSVDA,KEY+28        SAVE HEADER DISK ADDRESS                     
*                                                                               
*MNT380  DS    0H                                                               
*        CLC   KEY(RINVKRTP-RINVKEY),KEYSAVE   CHECK UP TO RECORD TYPE          
*        BNE   MNT440                                                           
*                                  SAVE END OF KEY AND DISK ADDRESS             
*        MVC   0(17,R2),KEY+RINVKSTD-RINVKEY                                    
*        MVC   17(4,R2),=CL4'BILL' END OF TABLE INDICATOR                       
*        OI    KEY+27,X'80'        DELETE OLD KEY                               
*        BAS   RE,MYDIRWRT                                                      
*        LA    R2,17(R2)           NEXT TABLE ENTRY                             
*        GOTO1 SEQ                 GET NEXT RECORD                              
*        BCT   R3,MNT380                                                        
*        DC    H'0'                OVER 100 RECORDS CHANGED TOO MANY            
*                                                                               
*  ADD THE NEW KEYS                                                             
*                                                                               
*MNT440  MVC   KEY,SAVEKEY                                                      
*        L     R2,AIO3                                                          
*MNT500  CLC   0(4,R2),=CL4'BILL'                                               
*        BE    MNT600                                                           
*        MVC   KEY+RINVKSTD-RINVKEY(12),0(R2)                                   
*        ZIC   R1,CHNGLEN                                                       
*        BCTR  R1,0                                                             
*        EX    R1,MOVEKEYS                                                      
*        OI    DMINBTS,X'08'                                                    
*        GOTO1 HIGH                                                             
*        NI    DMINBTS,X'F7'                                                    
*        CLC   KEYSAVE(27),KEY                                                  
*        BE    MNT560                                                           
*        MVC   KEY+RINVKSTD-RINVKEY(12),0(R2)                                   
*        ZIC   R1,CHNGLEN                                                       
*        BCTR  R1,0                                                             
*        EX    R1,MOVEKEYS                                                      
*        MVC   KEY+27(5),12(R2)                                                 
*        BAS   RE,MYDIRADD                                                      
*        B     MNT580                                                           
*        SPACE 1                                                                
*MNT560  MVC   KEY+27(5),12(R2)     MOVE DISK ADDRESS AND STATUS                
*        BAS   RE,MYDIRWRT                                                      
*        SPACE 1                                                                
*MNT580  LA    R2,17(R2)                                                        
*        B     MNT500                                                           
*                                                                               
*  READ THE NEW HEADER AND SAVE THE DISK ADDRESS FOR THE SECONDARY PTRS         
MNT600   MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BSVDA,KEY+28                                                     
*  REMOVE THE OLD PASSIVE POINTERS (R4 POINTS TO OLD RECORD)                    
         GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
*                                                                               
*  ADD THE PASSIVE POINTERS (R6 POINTS TO NEW RECORD)                           
         GOTO1 INVPTR,DMCB,0(R6),WORK2                                          
         GOTO1 NWPT,DMCB,WORK2                                                  
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
COMPKEYS CLC   KEY(0),KEYSAVE                                                   
MOVEKEYS MVC   KEY(0),SAVEKEY                                                   
         EJECT                                                                  
*              ROUTINE TO ADD PASSIVE POINTERS                                  
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
         SPACE 1                                                                
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    EXIT                END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRADD                                                      
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
         SPACE 2                                                                
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4                                                           
         OI    KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
         SPACE 2                                                                
*              ROUTINE TO RESTORE POINTERS                                      
         SPACE 1                                                                
RSTPT    NTR1                                                                   
         L     R2,0(R1)                                                         
RSTPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   RSTPT4                                                           
         NI    KEY+27,X'7F'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
RSTPT4   LA    R2,32(R2)                                                        
         B     RSTPT1                                                           
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
         SPACE 1                                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
         SPACE 1                                                                
INVPTX   B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         SPACE 1                                                                
*  THESE DAYPARTS ONLY GET INVENTORY NUMBER AND START DATE                      
*       ERATLF - THEY ARE THE FRINGE "SUB-DAYPARTS"                             
* (W-WEEKEND IS NOT TREATED AS FRINGE FOR PASSIVE POINTERS, BUT                 
*    IS GROUPED WITH FRINGE EVERYWHERE ELSE)                                    
         EJECT                                                                  
*              END OLD INVENTORY THAT HAS NO END DATE                           
*    AIO1-OLD REORD TO BE CHANGED                                               
*    AIO2-NEW RECORD JUST ADDED                                                 
         SPACE 1                                                                
ENDOLD   NTR1                                                                   
         CLI   DATEDEB,0                                                        
         BNE   EXIT                NEW RECORD HAS END                           
         OC    DATEDEB,DATEDEB                                                  
         BZ    EXIT                NO DATA EXIT                                 
         SPACE 1                                                                
         L     R4,AIO2             RECORD JUST ADDED                            
         L     R5,AIO1             RECORD TO BE CHANGED                         
         USING REINVREC,R5                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKSTD-RINVKEY),0(R4)                                      
         GOTO1 HIGH                                                             
         SPACE 1                                                                
ENDOLDA  DS    0H                                                               
         CLC   KEYSAVE(RINVKSTD-RINVKEY),0(R4)                                  
         BNE   ENDOLDX             NO MATCHING KEY                              
         SPACE 1                                                                
*                                  MUST BE A HEADER                             
         OC    KEY+RINVKRTP-RINVKEY(L'RINVKRTP),KEY+RINVKRTP-RINVKEY            
         BNZ   ENDOLDN                                                          
         SPACE 1                                                                
         CLC   KEY(27),0(R4)       THIS IS THE RECORD                           
         BE    ENDOLDN             I JUST ADDED                                 
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BNZ   ENDOLDN             THIS ONE HAS END DATE                        
         L     R5,AIO2             SET USING TO NEW RECORD                      
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(0,WORK)  START OF NEW ITEM             
         L     R5,AIO1             SET USING TO CHANGE RECORD                   
         SPACE 1                                                                
         MVC   HALF,DATEDEB        NUMBER OF DAYS TO DECREASE                   
         LH    R2,HALF                                                          
         LCR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RINVPEFF+2)                            
         SPACE 1                                                                
         CLC   RINVPEFF(2),RINVPEFF+2   THE END CAN NOT BE LOWER                
         BNH   *+10                     THAN THE START                          
         MVC   RINVPEFF+2(2),RINVPEFF                                           
         MVC   AIO,AIO1                                                         
         BAS   RE,MYFILWRT         WRITE RECORD OUT                             
         B     ENDOLDX                                                          
         SPACE 1                                                                
ENDOLDN  GOTO1 SEQ                                                              
         B     ENDOLDA                                                          
         SPACE 1                                                                
ENDOLDX  MVC   AIO,AIO2            RESET AIO                                    
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*              CHANGE OLD RECORDS IF KEY CHANGE                                 
*   R4 = ADDRESS OF THE OLD RECORD (AIO1)                                       
*   R6 = ADDRESS OF THE NEW RECORD (AIO2)                                       
         SPACE 1                                                                
CHAOLD   NTR1                                                                   
         MVC   KEY(27),0(R4)       OLD HEADER KEY                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         OI    KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT         DELETE OLD HEADER POINTER                    
         B     CHAOLDP                                                          
         SPACE 1                                                                
CHAOLDN  DS    0H                  IS THIS AN OLD BOOK OR TEXT                  
         CLC   KEYSAVE(RINVKRTP-RINVKEY),KEY                                    
         BNE   EXIT                NO, I AM FINISHED                            
         SPACE 1                                                                
         L     R2,AIO3                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         OI    KEY+27,X'80'        DELETE OLD POINTER                           
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
         MVC   0(RINVKRTP-RINVKEY,R2),0(R6)      NEW KEY                        
*  GET FIRST X'CE' ELEMENT AND SAVE THE FROM BOOK (FROM TRACK RECORD)           
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CE',AIO3),0                         
         CLI   12(R1),0                                                         
         BNE   CHAOLDO             NO X'CE' ELEM WRITE RECORD OUT               
         L     R5,12(R1)                                                        
         MVC   INVSRC(3),7(R5)                                                  
*  DELETE ALL THE X'CE' ELEMENTS (FROM THE TRACK RECORD)                        
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'CE',AIO3),0                         
*  GET FIRST X'02' DAY TIME ELEMENT (FROM THE INV HEADER)                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO2),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
*  BUILD A X'CE' ELEMENT                                                        
CHAOLDM  XC    WORK,WORK                                                        
         MVC   WORK(2),=X'CE0A'                                                 
         MVC   WORK+2(5),2(R5)     DAY TIME FROM X'02' ELEM                     
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,0                        
*  GET NEXT DAY TIME ELEMENT                                                    
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),X'02'                                                      
         BE    CHAOLDM                                                          
*                                                                               
CHAOLDO  BAS   RE,FLADD            ADD IT                                       
         SPACE 1                                                                
CHAOLDP  MVC   KEY(27),0(R4)       ITS DELETED SO I'LL GET NEXT                 
         GOTO1 HIGH                                                             
         B     CHAOLDN                                                          
         EJECT                                                                  
*              CHANGE DAY AND TIME ON THE TRACK RECORDS                         
*   R4 = ADDRESS OF THE OLD RECORD (AIO1)                                       
*   R6 = ADDRESS OF THE NEW RECORD (AIO2)                                       
         SPACE 1                                                                
CHADYTM  NTR1                                                                   
         CLI   ACTNUM,ACTCHA                                                    
         BNE   EXIT                                                             
         MVC   KEY(27),0(R4)       OLD HEADER KEY                               
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 GET FIRST TRACK                              
         SPACE 1                                                                
CHADT020 DS    0H                  IS THIS AN OLD BOOK OR TEXT                  
         CLC   KEYSAVE(RINVKRTP-RINVKEY),KEY                                    
         BNE   EXIT                NO, I AM FINISHED                            
         SPACE 1                                                                
         L     R2,AIO3                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE 1                                                                
*  GET FIRST X'CE' ELEMENT AND SAVE THE FROM BOOK (FROM TRACK RECORD)           
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CE',AIO3),0                         
         CLI   12(R1),0                                                         
         BNE   CHADT100            NO X'CE' ELEM WRITE RECORD OUT               
         L     R5,12(R1)                                                        
         MVC   INVSRC(3),7(R5)                                                  
*  DELETE ALL THE X'CE' ELEMENTS (FROM THE TRACK RECORD)                        
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'CE',AIO3),0                         
*  GET FIRST X'02' DAY TIME ELEMENT (FROM THE INV HEADER)                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO2),0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
*  BUILD A X'CE' ELEMENT                                                        
CHADT060 XC    WORK,WORK                                                        
         MVC   WORK(2),=X'CE0A'                                                 
         MVC   WORK+2(5),2(R5)     DAY TIME FROM X'02' ELEM                     
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,0                        
*  GET NEXT DAY TIME ELEMENT                                                    
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),X'02'                                                      
         BE    CHADT060                                                         
*                                                                               
CHADT100 BAS   RE,MYFILWRT         ADD IT                                       
         SPACE 1                                                                
         GOTO1 SEQ                                                              
         B     CHADT020                                                         
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
FLADD    NTR1                                                                   
         L     R6,AIO                                                           
         USING RINVAEL,R5                                                       
         USING REINVREC,R6                                                      
         MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
         MVC   RINVALST,RINVAFST                                                
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
         SPACE 1                                                                
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'EF',AIO3),0                         
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,=C'ADD=CODE'             
         BAS   RE,MYFILADD         ADD THE RECORD                               
         MVC   BSVDA,KEY                                                        
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     EXIT                                                             
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   *+12                                                             
         MVI   KEY+27,0                                                         
         BAS   RE,MYDIRWRT         UNDELETE THE POINTER                         
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         SPACE 1                                                                
*  GET X'EF' ELEMENT AND UPDATE THE CHANGE DATE                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'EF',AIO3),0                         
         CLI   12(R1),0                                                         
         BNE   FLP100              NO X'EF' ELEM BUILD ONE                      
         L     R5,12(R1)                                                        
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         MVI   RINVAWHY,C'C'                                                    
         B     FLP200                                                           
         SPACE 1                                                                
FLP100   GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,=C'ADD=CODE'             
         SPACE 1                                                                
FLP200   BAS   RE,MYFILWRT         WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         B     EXIT                                                             
         DROP  R5,R6                                                            
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
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  R9,RB                                                            
OVFLRTN  NMOD1 0,*RM10OV*                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     GETQTR                                                           
         B     ACTIVITY                                                         
         B     OVLIST                                                           
         EJECT                                                                  
         SPACE 3                                                                
* SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETQTR   L     R3,AIO                                                           
         USING REINVREC,R3                                                      
*                                                                               
         USING RIDTELEM,R6                                                      
         LA    R6,WORK2                                                         
*                                                                               
         CLI   FIRSTSW,C'Y'                                                     
         BNE   GETQTEX                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,RIDTTIME       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,BYTE                                                          
         PRINT GEN                                                              
         EDIT  (B1,BYTE),(2,RINVKQTR),FILL=0                                    
         MVC   RINVOINV(1),BYTE                                                 
         PRINT NOGEN                                                            
GETQTEX  B     OVEXIT                                                           
         DROP  R3,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CREATE ACTIVITY ELEMENT                                        
*                                                                               
ACTIVITY DS    0H                                                               
         USING RINVAEL,R3                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OVREP),(X'EF',AIO1),0                           
         CLI   12(R1),0                                                         
         BE    *+8                                                              
         B     ACTV100                                                          
         L     R3,12(R1)                                                        
*                                                                               
         MVI   RINVAWHY,C'C'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         B     ACTV200                                                          
*-- ADD NEW ACTIVITY ELEMENT                                                    
ACTV100  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=XL2'EF0C'                                           
         MVI   RINVAWHY,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
ACTV200  GOTO1 HELLO,DMCB,(C'P',OVREP),(0,AIO),(R3),0                           
         B     ACTVEX                                                           
         PRINT NOGEN                                                            
ACTVEX   B     OVEXIT                                                           
         DROP  R3                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              LIST RECORD ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
OVLIST   DS    0H                                                               
         OC    KEY(RINVKINV-RINVKEY),KEY                                        
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(RINVKRTP-RINVKEY),SAVEKEY                                    
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(RINVKINV-RINVKEY),KEY                                    
         BNE   LREXT                                                            
         CLI   RINVKRTP,X'00'      INVENTORY HEADER                             
         BNE   LR200                                                            
         CLI   RINVKINV+3,0        CHECK OLD RECORD FORMAT                      
         BE    LR200                                                            
         MVC   SAVEKEY,KEY                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*--DATE FILTER                                                                  
         CLI   DTEHLD2,0                                                        
         BE    LR260                                                            
         CLI   RINVPEFF+2,0        DATE RANGE                                   
         BE    LR240               NO SINGLE DATE                               
         CLC   DTEHLD2,RINVPEFF                                                 
         BL    LR200                                                            
         CLC   DTEHLD2,RINVPEFF+2                                               
         BH    LR200                                                            
         B     LR260                                                            
*--SINGLE DATE EDIT                                                             
LR240    CLC   DTEHLD2,RINVPEFF                                                 
         BH    LR200                                                            
*                                                                               
LR260    CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         LA    R2,INVINVH          INVENTORY                                    
         MVC   LINVNUM(4),RINVKINV                                              
*                                                                               
         LA    R3,RINVPEFF         EFFECTIVE DATES                              
         LA    R4,LEFFDTE                                                       
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R4))                                  
         CLI   2(R3),0                                                          
         BE    LR280                                                            
         MVI   8(R4),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,9(R4))                                  
*                                                                               
LR280    MVC   LDPT,RINVDP         DAYPART                                      
*                                                                               
*  DAY/TIME                                                                     
         USING RIDTELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',OVREP),(X'02',(R6)),0                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
*                                                                               
         GOTO1 UNDAY,DMCB,RIDTDAY,LDYTIME           DAY                         
         LA    RE,17                                                            
         LA    R3,LDYTIME                                                       
*                                                                               
LR320    CLI   0(R3),X'40'                                                      
         BNH   LR340                                                            
         LA    R3,1(R3)                                                         
         BCT   RE,LR320                                                         
         DC    H'0'                                                             
LR340    MVI   0(R3),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R3))       TIME                        
         DROP  R4                                                               
*                                                                               
*  PROGRAM                                                                      
         USING RIPGELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',OVREP),(X'03',(R6)),0                           
         CLI   12(R1),0                                                         
         BNE   LR500                                                            
         L     R4,12(R1)                                                        
         ZIC   R1,RIPGLEN                                                       
         S     R1,=F'2'                                                         
         C     R1,=F'20'           MAX OUTPUT SIZE                              
         BNH   *+8                                                              
         LA    R1,20                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     LR500                                                            
         MVC   LPROGRM(0),RIPGNAME                                              
*                                                                               
         SPACE                                                                  
LR500    GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     OVEXIT                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
* PRINTING THE LINE                                                             
PR       DS    0H                                                               
         SPACE                                                                  
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         XC    DYTMPTR(12),DYTMPTR     CHECK FOR OVERFLOW CONDITION             
*                                                                               
         LA    R4,P                                                             
         USING PLINED,R4                                                        
*                                                                               
         LA    R2,INVINVH          INVENTORY                                    
         MVC   PRINVNUM(4),RINVKINV                                             
*                                                                               
         LA    R3,RINVPEFF         EFFECTIVE DATES                              
         LA    R2,PREFFDTE                                                      
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R2))                                  
         CLI   2(R3),0                                                          
         BE    PR80                                                             
         MVI   8(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,9(R2))                                  
*                                                                               
PR80     MVC   PRDAYPT(6),RINVDP   DAYPART                                      
*                                                                               
*  AVAIL DAY TIME                                                               
*                                                                               
*        OC    RINVPADY,RINVPADY                                                
*        BZ    PR140                                                            
*        GOTO1 UNDAY,DMCB,RINVPADY,PRAVDYTM                                     
*        LA    RE,20                                                            
*        LA    R2,PRAVDYTM                                                      
*                                                                               
*PR120   CLI   0(R2),X'40'                                                      
*        BNH   PR140                                                            
*        LA    R2,1(R2)                                                         
*        BCT   RE,PR120                                                         
*        DC    H'0'                                                             
*PR140   OC    RINVPATM,RINVPATM                                                
*        BZ    PR220                                                            
*        MVI   0(R2),C'/'                                                       
*        GOTO1 UNTIME,DMCB,RINVPATM,(0,1(R2))  AVAIL TIME                       
*                                                                               
*  DAY/TIME                                                                     
PR220    LA    R4,P                                                             
         LA    R5,4                                                             
         CLI   OVFLSW,C'Y'                                                      
         BNE   PR240                                                            
         OC    DYTMPTR,DYTMPTR                                                  
         BZ    PR400                                                            
         L     R3,DYTMPTR          POINT R3 WHERE WE LEFT OFF                   
         XC    DYTMPTR,DYTMPTR                                                  
         B     PR300                                                            
         USING RIDTELEM,R3                                                      
PR240    GOTO1 HELLO,DMCB,(C'G',OVREP),(X'02',AIO),0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*                                                                               
PR300    GOTO1 UNDAY,DMCB,RIDTDAY,PRDYTIME          DAY                         
         LA    RE,20                                                            
         LA    R2,PRDYTIME                                                      
*                                                                               
PR320    CLI   0(R2),X'40'                                                      
         BNH   PR340                                                            
         LA    R2,1(R2)                                                         
         BCT   RE,PR320                                                         
         DC    H'0'                                                             
PR340    MVI   0(R2),C'/'                                                       
         GOTO1 UNTIME,DMCB,RIDTTIME,(0,1(R2))       TIME                        
         ZIC   RE,RIDTLEN                                                       
         AR    R3,RE                                                            
         CLI   0(R3),X'02'                                                      
         BNE   PR400                                                            
         LA    R4,132(R4)          BUMP TO NEXT LINE                            
         BCT   R5,PR300                                                         
         ST    R3,DYTMPTR                                                       
         DROP  R3                                                               
*                                                                               
*  PROGRAM                                                                      
PR400    LA    R4,P                                                             
         LA    R5,4                                                             
         CLI   OVFLSW,C'Y'                                                      
         BNE   PR440                                                            
         OC    PROGPTR,PROGPTR                                                  
         BZ    PR500                                                            
         L     R3,PROGPTR          POINT R3 WHERE WE LEFT OFF                   
         XC    PROGPTR,PROGPTR                                                  
         B     PR460                                                            
*                                                                               
         USING RIPGELEM,R3                                                      
PR440    GOTO1 HELLO,DMCB,(C'G',OVREP),(X'03',AIO),0                            
         CLI   12(R1),0                                                         
         BNE   PR500                                                            
         L     R3,12(R1)                                                        
*                                                                               
PR460    ZIC   R1,RIPGLEN                                                       
         S     R1,=F'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRPRGNM(0),RIPGNAME                                              
         ZIC   RE,RIPGLEN                                                       
         AR    R3,RE                                                            
         CLI   0(R3),X'03'                                                      
         BNE   PR500                                                            
         LA    R4,132(R4)          BUMP TO NEXT LINE                            
         BCT   R5,PR460                                                         
         ST    R3,PROGPTR                                                       
*                                                                               
*  AVAIL DAY/TIME                                                               
PR500    LA    R4,P                                                             
         LA    R5,4                                                             
         CLI   OVFLSW,C'Y'                                                      
         BNE   PR540                                                            
         OC    AVPRPTR,AVPRPTR                                                  
         BZ    PR600                                                            
         L     R3,AVPRPTR          POINT R3 WHERE WE LEFT OFF                   
         XC    AVPRPTR,AVPRPTR                                                  
         B     PR560                                                            
*                                                                               
         USING RIAPELEM,R3                                                      
PR540    GOTO1 HELLO,DMCB,(C'G',OVREP),(X'04',AIO),0                            
         CLI   12(R1),0                                                         
         BNE   PR600                                                            
         L     R3,12(R1)                                                        
*                                                                               
PR560    MVC   PRAVDAY,RIADAY                                                   
         MVC   PRAVTIME,RIATIME                                                 
         ZIC   RE,RIAPLEN                                                       
         AR    R3,RE                                                            
         CLI   0(R3),X'04'                                                      
         BNE   PR600                                                            
         LA    R4,132(R4)          BUMP TO NEXT LINE                            
         BCT   R5,PR560                                                         
         ST    R3,AVPRPTR                                                       
*                                                                               
PR600    LA    R4,P                RESET R4                                     
         GOTO1 SPOOL,DMCB,(R7)                                                  
         OC    DYTMPTR(12),DYTMPTR     CHECK FOR OVERFLOW CONDITION             
         BZ    PR660                                                            
         MVI   OVFLSW,C'Y'                                                      
         B     PR220                                                            
*  NO OVERFLOW CONDITION                                                        
PR660    MVI   OVFLSW,C'N'                                                      
         B     LR200                                                            
         SPACE                                                                  
PREXT    DS    0H                                                               
         B     OVEXIT                                                           
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H3,1,C'STATION -'                                                
         SSPEC H1,49,C'INVENTORY LISTING'                                       
         SSPEC H2,49,C'-----------------'                                       
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HDRTN    NTR1                                                                   
         LA    R2,INVSSTAH                                                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+11(0),STAHLD     STATION                                      
         SPACE 1                                                                
*                                                                               
         MVC   H8,SPACES                                                        
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
         MVC   PRINVNUM,=C'INV#'                                                
         MVC   PRINVNUM+132(4),=4C'-'                                           
         MVC   PREFFDTE+1(14),=C'EFFECTIVE DATE'                                
         MVC   PREFFDTE+132(17),=17C'-'                                         
         MVC   PRPRGNM+7(12),=C'PROGRAM NAME'                                   
         MVC   PRPRGNM+132(27),=27C'-'                                          
         MVC   PRDAYPT(7),=C'DAYPART'                                           
         MVC   PRDAYPT+132(7),=7C'-'                                            
         MVC   PRDYTIME+6(8),=C'DAY/TIME'                                       
         MVC   PRDYTIME+132(20),=20C'-'                                         
         MVC   PRAVDAY+1(9),=C'AVAIL DAY'                                       
         MVC   PRAVDAY+132(11),=11C'-'                                          
         MVC   PRAVTIME(10),=C'AVAIL TIME'                                      
         MVC   PRAVTIME+132(11),=11C'-'                                         
*        MVC   PRAVDYTM+3(14),=C'AVAIL DAY/TIME'                                
*        MVC   PRAVDYTM+132(20),=20C'-'                                         
*        MVC   PRAVPROG+5(13),=C'AVAIL PROGRAM'                                 
*        MVC   PRAVPROG+132(24),=24C'-'                                         
         B     OVEXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 3                                                                
OVEXIT   XMOD1 1                                                                
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
OVREP    DC    CL8'REPFILE'                                                     
         EJECT                                                                  
PLINED   DSECT                                                                  
PRINVNUM DS    CL4                                                              
         DS    CL3                                                              
PREFFDTE DS    CL17                                                             
         DS    CL3                                                              
PRPRGNM  DS    CL27                                                             
         DS    CL3                                                              
PRDYTIME DS    CL20                                                             
         DS    CL3                                                              
PRDAYPT  DS    CL7                                                              
         DS    CL3                                                              
PRAVDAY  DS    CL11                                                             
         DS    CL3                                                              
PRAVTIME DS    CL11                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LINVNUM  DS    CL6                                                              
         DS    CL1                                                              
LEFFDTE  DS    CL17                                                             
         DS    CL1                                                              
LPROGRM  DS    CL20                                                             
         DS    CL1                                                              
LDPT     DS    CL6                                                              
         DS    CL1                                                              
LDYTIME  DS    CL18                                                             
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
       ++INCLUDE REGAVD7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVD8D                                                       
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGAVWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
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
INVHLD   DS    CL4                 INVENTORY HOLD AREA                          
DTEHLD   DS    CL3                 DATE HOLD AREA                               
DTEHLD2  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE2 DS    CL2                 2 BYTE END DATE HOLD                         
*                                                                               
DATEDEB  DS    CL2                 DAY TO SUBTRACT FROM LAST INV                
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
REPAGNCY DS    CL2                 REP AGENCY                                   
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
         EJECT                                                                  
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
**PAN#1  DC    CL21'012REGAV10   04/29/09'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
