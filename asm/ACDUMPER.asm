*          DATA SET ACDUMPER   AT LEVEL 023 AS OF 02/23/07                      
*PHASE ACDUMPA,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SCANNER                                                                
*INCLUDE STXITER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'ACCPAK DUMP ACC RECORDS'                                        
         PRINT NOGEN                                                            
ACDUMP   CSECT                                                                  
         NBASE 0,*DUM*,=V(REGSAVE)                                              
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         GOTO1 STXITER,DMCB,DMPL1                                               
         MVI   OUTPUT,0                                                         
                                                                                
INIT1    GOTO1 CARDS,DMCB,CARDIO,=C'RE00'                                       
         MVC   P(L'CARDIO),CARDIO                                               
         GOTO1 PRINTER                                                          
         CLC   CARDIO(2),=C'/*'                                                 
         BE    ACDMP                                                            
         LA    R0,L'CARDIO                                                      
         LA    RF,CARDIO+L'CARDIO-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
                                                                                
         STC   R0,CARDH+5          SET LENGTH OF INPUT                          
         AHI   R0,L'CARDH                                                       
         STC   R0,CARDH                                                         
         L     R3,AINP                                                          
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(40,CARDH),(10,SCAND),0                             
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          SET NUMBER OF PARAMETERS                     
         BNZ   *+6                                                              
         DC    H'0'                BAD CARD                                     
*                                                                               
INIT3    LA    R2,OPTTAB           LIST OF VALID INPUT FIELDS                   
         SR    R1,R1                                                            
*                                                                               
INIT5    IC    R1,0(R2)            LENGTH FOR COMPARE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCANLFT(0),1(R2)    MATCH CARD FIELD TO TABLE                    
         BE    INIT7                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),EOT                                                        
         BNE   INIT5                                                            
         DC    H'0'                INVALID INPUT OPTION                         
*                                                                               
INIT7    SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,15,12(R2)                                                     
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         AHI   R3,SCANLNQ                                                       
         BCT   R0,INIT3                                                         
         B     INIT1               GET NEXT CARD                                
         DROP  R3,R8                                                            
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS, SEE IF RANGE OF RECORDS                                         
***********************************************************************         
ACDMP    BAS   RE,OPNF             OPEN INPUT FILE                              
         TM    OUTPUT,TAPE                                                      
         BZ    ACDMP20                                                          
         L     R3,ATOUT                                                         
         OPEN  ((R3),OUTPUT)                                                    
         TM    48(R3),X'10'        OPEN OKAY ?                                  
         BO    ACDMP20                                                          
         ABEND 206,DUMP                                                         
                                                                                
ACDMP20  ICM   R0,15,MAXDMP                                                     
                                                                                
ACDMP30  BAS   RE,GETIN            GET INPUT RECORD                             
         TM    DMBYTE,X'80'        TEST EOF                                     
         BO    CLOSE                                                            
                                                                                
         L     R2,AINP                                                          
         CLI   #OFKEYS,1                                                        
         BE    ACDMP50                                                          
                                                                                
***********************                                                         
* MULTIPLE KEY PAIRS  *                                                         
***********************                                                         
         LA    R1,1                                                             
ACDMP40  LR    RF,R1                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'KEYSTRT                                                     
         LA    RE,KEYSTRT(RF)                                                   
         CLC   0(L'KEYSTRT,R2),0(RE)                                            
         BL    ACDMP48                                                          
                                                                                
         LA    RE,KEYEND(RF)                                                    
         CLC   0(L'KEYEND,R2),0(RE)                                             
         BNH   ACDMP80             WAS VALID KEY                                
         CLM   R1,1,#OFKEYE                                                     
         BNL   CLOSE               DONE                                         
                                                                                
ACDMP48  AHI   R1,1                                                             
         CLM   R1,1,#OFKEYS                                                     
         BH    ACDMP30             GET NEXT RECORD                              
         B     ACDMP40             TRY NEXT PAIR OF KEYS                        
****************************                                                    
* SINGLE START AND/OR END  *                                                    
****************************                                                    
ACDMP50  TM    INPUT,START         TEST START=XXX                               
         BZ    ACDMP52                                                          
         CLC   0(L'KEYSTRT,R2),KEYSTRT                                          
         BL    ACDMP30                                                          
                                                                                
ACDMP52  TM    INPUT,END           TEST END=XXX                                 
         BZ    ACDMP80                                                          
         CLC   0(L'KEYEND,R2),KEYEND                                            
         BH    CLOSE                                                            
                                                                                
ACDMP80  BAS   RE,PDUMP            PDUMP OF RECORDS                             
         BAS   RE,PUTOUT                                                        
         BCT   R0,ACDMP30                                                       
*                                                                               
CLOSE    TM    INPUT,TAPE                                                       
         BZ    CLOSE10                                                          
         CLOSE (TAPEIN)                                                         
                                                                                
CLOSE10  TM    OUTPUT,TAPE                                                      
         BZ    EOJ                                                              
         CLOSE (TAPEOUT)                                                        
*                                                                               
EOJ      XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY CODE                                                *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VAGYC    MVC   ALPHA,SCANRHT       AGENCY=XX                                    
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT TYPE                                               *          
***********************************************************************         
         USING SCAND,R3                                                         
VOUTC    DS    0H                                                               
         CLC   SCANRHT(4),=C'TAPE'                                              
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    OUTPUT,TAPE                                                      
         BR    RE                                                               
         DROP  R3                                                               
***********************************************************************         
* VALIDATE INPUT TYPE                                                *          
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VINPC    NI    INPUT,ALL-(TAPE+DISK)                                            
         CLC   SCANRHT(4),=C'TAPE'                                              
         BNE   *+10                                                             
         OI    INPUT,TAPE                                                       
         BR    RE                                                               
         CLC   SCANRHT(4),=C'DISK'                                              
         BNE   *+10                                                             
         OI    INPUT,DISK                                                       
         BR    RE                                                               
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE START / END CARD                                           *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VUPSI    NTR1                                                                   
         CLI   SCANRLN,8           8 FOR BYTE OF BITS                           
         BE    *+6                                                              
         DC    H'00'               UPSI=00000000                                
                                                                                
         LA    R0,X'80'                                                         
         LA    R1,8                                                             
         SR    RF,RF                                                            
         LA    R2,SCANRHT          "00000000" PORTION                           
VUPSI10  CLI   0(R2),C'0'                                                       
         BE    VUPSI20                                                          
         CLI   0(R2),C'1'                                                       
         BE    *+6                                                              
         DC    H'00'               HAS TO BE 0 OR 1                             
         OR    RF,R0               OR ON BIT                                    
                                                                                
VUPSI20  SRL   R0,1                SHIFT BIT BY ONE                             
         AHI   R2,1                MOVE UP IN STRING                            
         BRCT  R1,VUPSI10                                                       
         STC   RF,UPSI                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE START / END CARD                                           *         
***********************************************************************         
                                                                                
VSTRC    NTR1  ,                                                                
         OI    INPUT,START         START=XXXXXXXX                               
         SR    RF,RF                                                            
         IC    RF,#OFKEYS                                                       
         AHI   RF,1                                                             
         CHI   RF,MAXKEYS                                                       
         BNH   *+6                                                              
         DC    H'00'                                                            
                                                                                
         STC   RF,#OFKEYS                                                       
         BCTR  RF,0                                                             
         MHI   RF,L'KEYSTRT                                                     
         LA    R4,KEYSTRT(RF)                                                   
         LA    R3,CARDIO+6                                                      
         B     VKEY                                                             
*                                                                               
VENDC    NTR1  ,                                                                
         OI    INPUT,END           END=XXXXXXXX                                 
         SR    RF,RF                                                            
         IC    RF,#OFKEYE                                                       
         AHI   RF,1                                                             
         CHI   RF,MAXKEYS          MAX NUMBER OF KEY PAIRS                      
         BNH   *+6                                                              
         DC    H'00'                                                            
                                                                                
         STC   RF,#OFKEYE                                                       
         CLC   #OFKEYS,#OFKEYE     MUST HAVE EQUAL #                            
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BCTR  RF,0                                                             
         MHI   RF,L'KEYEND                                                      
         LA    R4,KEYEND(RF)                                                    
         LA    R3,CARDIO+4                                                      
*                                                                               
VKEY     GOTO1 DECODE,DMCB,(42,(R3)),(X'00',(R4))                               
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE MAXDUMP                                                    *         
***********************************************************************         
                                                                                
         USING SCAND,R3                                                         
VMXDMP   NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MAXDMP,SCANRBV                                                   
         B     XIT                                                              
***********************************************************************         
* BLOCK SIZE                                                                    
***********************************************************************         
VBLKSZ   NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   BLKSZ,SCANRBV                                                    
         B     XIT                                                              
***********************************************************************         
* RECORD LENGTH                                                                 
***********************************************************************         
VRECL    NTR1  ,                                                                
         OC    SCANRBV,SCANRBV     MAXDUMP=FULL WORD                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   RECLN,SCANRBV                                                    
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN CONTROL FILE AND GET SE NUMBER                                 *         
***********************************************************************         
                                                                                
OPNF     NTR1  ,                                                                
         TM    INPUT,TAPE          TEST INPUT TAPE                              
         BNO   OPNF3                                                            
         L     R3,ATINT                                                         
         MVC   62(L'BLKSZ,R3),BLKSZ                                             
         MVC   82(L'RECLN,R3),RECLN                                             
         OPEN  (TAPEIN,(INPUT))                                                 
         B     XIT                                                              
*                                                                               
OPNF3    GOTO1 DATAMGR,DMCB,OPEN,CONTROL,CTFILEL                                
         LA    R2,DKEY             READ ACCESS RECORD                           
         USING CT5REC,R2                                                        
         XC    DKEY,DKEY                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,ALPHA                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,AINP                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AINP                                                          
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
*                                                                               
         USING CTSYSD,R3                                                        
OPNF5    CLI   0(R3),0             FIND SYSTEM ELEMENT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTSYSELQ                                                   
         BNE   *+12                                                             
         CLI   CTSYSNUM,X'06'      TEST ACCOUNT FILE                            
         BE    OPNF7                                                            
         IC    R0,1(,R3)                                                        
         AR    R3,R0                                                            
         B     OPNF5                                                            
*                                                                               
OPNF7    MVC   SE,CTSYSSE          GET  SE NUMBER                               
         GOTO1 DATAMGR,DMCB,OPEN,ACCOUNT,ACFILEL                                
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET INPUT RECORD                                                    *         
***********************************************************************         
PUTOUT   NTR1  ,                                                                
         TM    OUTPUT,TAPE                                                      
         BZ    PUTOUTX                                                          
         L     R3,ATOUT                                                         
         L     R5,AINPL                                                         
         PUT   (R3),(R5)                                                        
                                                                                
PUTOUTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET INPUT RECORD                                                    *         
***********************************************************************         
                                                                                
GETIN    NTR1  ,                                                                
         TM    INPUT,DISK          TEST INPUT=DISK                              
         BO    GETIND                                                           
         L     R3,ATINT                                                         
         L     R5,AINPL                                                         
         GET   (R3),(R5)                                                        
         B     *+8                                                              
GETINX   MVI   DMBYTE,X'80'        SET EOF FLAG                                 
         B     XIT                                                              
*                                                                               
GETIND   TM    INPUT,NEXT          TEST FIRST TIME                              
         BO    GETIND5                                                          
         MVC   DKEY,KEYSTRT                                                     
         GOTO1 ADMGR,DMHI                                                       
         OI    INPUT,NEXT                                                       
         B     GETIND7                                                          
*                                                                               
GETIND5  GOTO1 ADMGR,DMSEQ                                                      
         TM    DMBYTE,X'80'        TEST EOF                                     
         BO    XIT                                                              
GETIND7  L     R2,AINP                                                          
         GOTO1 ADMGR,DMGET         GET THE RECORD                               
         CLC   0(42,R2),DIR        TEST RECORD = KEY                            
         BNE   GETIND5             MUST BE PASSIVE                              
         SR    RE,RE                                                            
         ICM   RE,3,ACCRLEN-ACCRECD(R2)                                         
         LA    RE,4(RE)            SET LENGTH                                   
         L     RF,AINPL                                                         
         STCM  RE,3,0(RF)                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PDUMP                                                               *         
***********************************************************************         
                                                                                
PDUMP    NTR1  ,                                                                
         AP    RECCNT,=P'1'                                                     
         OI    RECCNT+(L'RECCNT-1),X'0F'                                        
         UNPK  PCAP,RECCNT                                                      
         L     RE,AINPL                                                         
         TM    UPSI,UPSIDMPR       DUMP AS RECORD/ELEMENTS                      
         BO    PDUMP10                                                          
         STCM  RE,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         GOTO1 PRNTBL,PDMP                                                      
         B     PDUMPX                                                           
                                                                                
PDUMP10  L     R2,AINPL                                                         
         LH    RF,0(,R2)           LENGTH OF RECORD                             
         MVC   WORK(4),=C'LEN='                                                 
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK+4(6),DUB                                                    
         AHI   R2,4                                                             
         LA    R3,ACCRFST-ACCRECD                                               
         GOTO1 PRNTBL,PARM,(10,WORK),(R2),C'DUMP',(R3),=C'1R',V(PRINT)          
                                                                                
PDUMPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                   *         
***********************************************************************         
                                                                                
DMGR     NTR1  ,                                                                
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     DMGREAD                                                          
         B     DMGHIGH                                                          
         B     DMGSEQ                                                           
         B     DMGGET                                                           
*                                                                               
DMGREAD  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGHIGH  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGSEQ   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGGET   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   DMBYTE,8(R1)                                                     
         NI    DMBYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED             
         TM    DMBYTE,X'80'        PASS BACK EOF                                
         BO    XIT                                                              
         CLI   DMBYTE,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
CARDS    DC    V(CARDS)                                                         
CPRINT   DC    V(CPRINT)                                                        
DATAMGR  DC    V(DATAMGR)                                                       
DECODE   DC    V(DECODE)                                                        
HEXIN    DC    V(HEXIN)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
PDUMPER  DC    V(PDUMPER)                                                       
PRINT    DC    V(PRINT)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SCANNER  DC    V(SCANNER)                                                       
STXITER  DC    V(STXITER)                                                       
DUMMY    DC    V(DUMMY)                                                         
*                                                                               
ADMGR    DC    A(DMGR)                                                          
DMRD     EQU   1                   READ                                         
DMHI     EQU   2                   HIGH                                         
DMSEQ    EQU   3                   SEQUENTIAL                                   
DMGET    EQU   4                   GET RECORD                                   
*                                                                               
AINPL    DC    A(INPL)             INPUT                                        
AINP     DC    A(INP)                                                           
ATINT    DC    A(TAPEIN)                                                        
ATOUT    DC    A(TAPEOUT)                                                       
*                                                                               
ALPHA    DC    CL2'  '                                                          
*                                                                               
PARM     DC    6F'0'                                                            
DMCB     DC    6F'0'                                                            
FULL     DC    F'0'                                                             
DUB      DC    D'0'                                                             
HALF     DC    H'0'                                                             
BYTE     DC    X'00'                                                            
WORK     DC    CL100' '                                                         
*                                                                               
CARDH    DS    XL8                 HEADER FOR CARD                              
         ORG   CARDH                                                            
         DC    AL1(L'CARDH+L'CARDIO)                                            
         DC    XL4'00'                                                          
         DC    AL1(L'CARDIO)                                                    
         DC    XL2'00'                                                          
CARDIO   DC    CL80' '                                                          
*                                                                               
#OFKEYS  DC    X'00'               NUMBER OF KEY STARTS                         
#OFKEYE  DC    X'00'               NUMBER OF KEY ENDS                           
KEYSTRT  DC    (MAXKEYS)XL42'00'                                                
KEYEND   DC    (MAXKEYS)XL42'FF'                                                
*                                                                               
UPSI     DS    X                                                                
UPSIDMPR EQU   X'80'                                                            
*                                                                               
DKEY     DS    CL42                                                             
DIR      DS    CL60                                                             
DA       DS    F                                                                
*                                                                               
DMWORK   DC    24F'0'                                                           
DMBYTE   DC    X'00'                                                            
SPACE    DC    CL132' '                                                         
*                                                                               
         ENTRY UTL                                                              
UTL      DC    F'0',X'0A'                                                       
         ORG   UTL+4                                                            
SE       DS    X                                                                
*                                                                               
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
OPEN     DC    C'OPEN   '                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
CTFILEL  DC    C'NCTFILE X'                                                     
ACCOUNT  DC    C'ACCOUNT'                                                       
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
*                                                                               
INPUT    DC    AL1(TAPE)                                                        
OUTPUT   DC    AL1(0)                                                           
DISK     EQU   X'80'                                                            
TAPE     EQU   X'40'                                                            
START    EQU   X'08'                                                            
END      EQU   X'04'                                                            
NEXT     EQU   X'01'                                                            
*                                                                               
DMPL1    DC    AL4(ACDUMP),X'80',VL3(DUMMY)                                     
MAXDMP   DC    F'500'                                                           
BLKSZ    DC    H'32760'                                                         
RECLN    DC    H'4004'                                                          
RECCNT   DC    PL6'0'                                                           
*                                                                               
PDMP     DS    0F                  PRNTBL PARAMETERS                            
PDMP1    DS    F                                                                
         ORG   PDMP1                                                            
PDMPLC   DC    AL1(L'PCAP)         LENGTH OF CAPTION                            
PDMPAC   DC    AL3(PCAP)           ADDRESS OF CAPTION                           
PDMP2    DS    F                                                                
         ORG   PDMP2                                                            
PDMPREC  DC    AL4(0)              ADDRESS OF RECORD                            
PDMP3    DS    CL4                                                              
         ORG   PDMP3                                                            
PDMPDMP  DC    C'DUMP'             CONSTANT                                     
PDMP4    DS    F                                                                
         ORG   PDMP4                                                            
PDMPLREC DC    AL4(0)              LENGTH OF RECORD                             
PDMP5    DS    F                                                                
         ORG   PDMP5                                                            
PDMP2D   DC    AL4(P2D)            CONSTANT A(2D)                               
PDMP6    DS    F                                                                
         ORG   PDMP6                                                            
PDMPP    DC    C'P'                CONSTANT 'P'                                 
PDMPPRT  DC    VL3(PRINT)          A(PRINT)                                     
*                                                                               
PCAP     DS    CL6                                                              
P2D      DC    C'2D'                                                            
*                                                                               
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
EFFS     DC    X'FFFFFFFFFFFFFF'                                                
MAXKEYS  EQU   6                                                                
*                                                                               
OPTTAB   DS    0CL16               CONTROL CARDS                                
         DC    AL1(5),CL11'AGENCY    ',AL4(VAGYC)                               
         DC    AL1(4),CL11'INPUT     ',AL4(VINPC)                               
         DC    AL1(5),CL11'OUTPUT    ',AL4(VOUTC)                               
         DC    AL1(4),CL11'START     ',AL4(VSTRC)                               
         DC    AL1(2),CL11'END       ',AL4(VENDC)                               
         DC    AL1(6),CL11'MAXDUMP   ',AL4(VMXDMP)                              
         DC    AL1(3),CL11'UPSI      ',AL4(VUPSI)                               
         DC    AL1(4),CL11'BLOCK     ',AL4(VBLKSZ)                              
         DC    AL1(2),CL11'LEN       ',AL4(VRECL)                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* DCB AND IO AREA                                                     *         
***********************************************************************         
                                                                                
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=GETINX,         +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
                                                                                
INPL     DC    F'0'                                                             
INP      DS    XL4004                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR SCANNER BLOCK                                                       
***********************************************************************         
                                                                                
SCAND    DSECT                                                                  
SCANLLN  DS    X                   LENGTH OF LEFT SIDE                          
SCANRLN  DS    X                   LENGTH OF RIGHT SIDE                         
SCANLST  DS    X                   LEFT STATUS BYTES                            
SCANNUM  EQU   X'80'               NUMERIC                                      
SCANALP  EQU   X'40'               ALPHA                                        
SCANHEX  EQU   X'20'               HEX                                          
SCANRST  DS    X                   RIGHT STATUS BYTES                           
SCANLBV  DS    XL4                 LEFT BINARY VALUE                            
SCANRBV  DS    XL4                 RIGHT BINARY VALUE                           
SCANLFT  DS    CL10                LEFT DATA                                    
SCANRHT  DS    CL40                RIGHT DATA                                   
SCANLNQ  EQU   *-SCAND                                                          
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACDUMPER  02/23/07'                                      
         END                                                                    
