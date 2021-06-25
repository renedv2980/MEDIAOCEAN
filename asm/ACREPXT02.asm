*          DATA SET ACREPXT02  AT LEVEL 010 AS OF 07/28/03                      
*PHASE ACXT02A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE DECODE                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'DUMP/COMPARE - RECOVERY/ACCOUNT FILE'                           
ACXT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXT**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXTD,RC                                                         
*                                                                               
         CLI   MODE,PROCSPEC       GET INPUT CARDS                              
         BE    INIT                                                             
         CLI   MODE,PROCRCVR       PROCESS A RECOVERY RECORD                    
         BE    PRCV                                                             
         CLI   MODE,RUNFRST        PROCESS FILE                                 
         BE    PFIL                                                             
         CLI   MODE,RUNLAST        PROCESS LAST                                 
         BE    CLOSE                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE - GET INPUT CARDS                                        *         
***********************************************************************         
                                                                                
INIT     DS    0H                                                               
*                                                                               
INIT1    GOTO1 CARDS,DMCB,CARDIO,=C'RE00'                                       
         MVC   P(L'CARDIO),CARDIO                                               
         GOTO1 ACREPORT                                                         
         CLC   CARDIO(2),=C'/*'                                                 
         BE    INIT7                                                            
*                                                                               
         LA    R2,OPTTAB           LIST OF VALID INPUT FIELDS                   
         SR    R1,R1                                                            
*                                                                               
INIT3    IC    R1,0(R2)            LENGTH FOR COMPARE                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CARDIO(0),1(R2)     MATCH CARD FIELD TO TABLE                    
         BE    INIT5                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   INIT3                                                            
         B     INITERR             INVALID INPUT OPTION                         
*                                                                               
INIT5    LA    R4,CARDIO           R4=RIGHT SIDE DATA                           
         LA    R4,1(R1,R4)                                                      
*                                                                               
         SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,15,12(R2)                                                     
         BR    RF                  VALIDATE INPUT OPTION                        
*                                                                               
INITERR  MVC   P+1(20),=CL20'INVALID OPTION CARD'                               
         GOTO1 ACREPORT                                                         
         DC    H'0'                                                             
*                                                                               
INIT7    TM    FILOPT,FILRCV+FILTOD+FILACC                                      
         BNZ   *+8                                                              
         OI    FILOPT,FILTOD       DEFAULT IS TAPE/DISK FILE                    
         TM    FILOPT,FILRCV                                                    
         BNO   *+8                                                              
         MVI   FCRDRCVR,C'D'       READ RECOVERY/INCLUDE DELETED                
*                                                                               
         SR    R2,R2                                                            
         TM    FILOPT,FILTOD                                                    
         BNO   *+8                                                              
         L     R2,ARCVTAPE         READ RECOVERY TAPE                           
*                                                                               
         TM    FILOPT,FILACC                                                    
         BNO   *+8                                                              
         L     R2,ATINT            READ ACCOUNT FILE                            
*                                                                               
         LTR   R2,R2               TEST FILE TO OPEN                            
         BZ    INIT9                                                            
         OPEN  ((R2),(INPUT))      OPEN FILE                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT9    TM    FILOPT,FILOUT       TEST OUTPUT FILE NEEDED                      
         BNO   INIT11                                                           
         OPEN  (TOUT,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT11   DS    0H                                                               
INITX    B     XIT                                                              
*                                                                               
OPTTAB   DS    0CL16                                                            
         DC    AL1(4),CL11'SIN=       ',AL4(VSIN)                               
         DC    AL1(4),CL11'KEY=       ',AL4(VKEY)                               
         DC    AL1(5),CL11'TYPE=      ',AL4(VTYP)                               
         DC    AL1(6),CL11'INPUT=     ',AL4(VINP)                               
         DC    AL1(6),CL11'HIKEY=     ',AL4(VHIK)                               
         DC    AL1(6),CL11'FILES=     ',AL4(VFIL)                               
         DC    AL1(6),CL11'PDUMP=     ',AL4(VPDU)                               
         DC    AL1(7),CL11'OUTPUT=    ',AL4(VOUT)                               
         DC    AL1(7),CL11'LOWKEY=    ',AL4(VLOW)                               
         DC    AL1(7),CL11'USERID=    ',AL4(VUSR)                               
         DC    AL1(8),CL11'TERMNUM=   ',AL4(VTRM)                               
         DC    AL1(8),CL11'LOWTIME=   ',AL4(VLTI)                               
         DC    AL1(8),CL11'DISKADR=   ',AL4(VDSK)                               
         DC    AL1(9),CL11'HIGHTIME=  ',AL4(VHTI)                               
         DC    AL1(9),CL11'PROGRAMS=  ',AL4(VPGM)                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALID OPTION CARDS                                                  *         
***********************************************************************         
                                                                                
VINP     CLI   0(R4),C'A'           INPUT=A (ACCOUNT)                           
         BNE   *+12                                                             
         OI    FILOPT,FILACC                                                    
         B     VOPTY                                                            
         CLI   0(R4),C'R'           INPUT=R (ACCRCV)                            
         BNE   *+12                                                             
         OI    FILOPT,FILRCV                                                    
         B     VOPTY                                                            
         CLI   0(R4),C'T'           INPUT=T (TAPE OR DISK)                      
         BNE   VOPTN                                                            
         OI    FILOPT,FILTOD                                                    
         B     VOPTY                                                            
                                                                                
*                                   KEY FIELDS                                  
VKEY     LA    R3,KYF               KEY=XXX                                     
         B     VKALL                                                            
VLOW     LA    R3,LOWK              LOWKEY=XXXXX                                
         B     VKALL                                                            
VHIK     LA    R3,HIK               HIKEY=XXXXXX                                
VKALL    GOTO1 DECODE,DMCB,(R4),0(R3)                                           
         BCTR  R3,0                                                             
         MVC   0(1,R3),11(R1)       SAVE LENGTH                                 
         B     VOPTY                                                            
                                                                                
*                                                                               
         USING SCAND,R3                                                         
VSIN     L     R3,AIO1              SIN=000000                                  
         GOTO1 SCANNER,DMCB,(C'C',CARDIO),(R3)                                  
         CLI   4(R1),1                  ERROR                                   
         BNE   VOPTN                                                            
         SR    R4,R4                                                            
         ICM   R4,1,SCANRLN         TEST LENGTH OF INPUT                        
         BZ    VOPTN                NO INPUT                                    
         CH    R4,=H'6'             TEST LENGTH MORE THAN 6                     
         BH    VOPTN                                                            
         GOTO1 HEXIN,DMCB,SCANRHT,FULL,(R4)                                     
         ICM   RF,15,12(R1)         RF=LENGTH OF DESTINATION                    
         BZ    VOPTN                                                            
         ICM   R4,7,FULL            R4=HEX DATA                                 
         LA    RE,3                 RE=MAX NUMBER OF BYTES                      
         SR    RE,RF                RE=BYTES TO SHIFT                           
         SLL   RE,3                 RE= LENGTH X 8 = BITS                       
         STCM  RE,3,*+6               SHIFT SIN                                 
         SRL   R4,0                                                             
         STCM  R4,7,SIN             SAVE INPUT NUMBER                           
         B     VOPTY                                                            
         EJECT                                                                  
VFIL     LA    R5,FILES             FILES=##,##,##                              
         B     *+8                                                              
VPGM     LA    R5,PRGMS             PROGRAMS=##,##                              
VFIL3    GOTO1 HEXIN,DMCB,0(R4),0(R5),2                                         
         ICM   RF,15,12(R1)         RF=LENGTH OF DESTINATION                    
         BZ    VOPTN                                                            
         CH    RF,=H'1'             CAN ONLY BE 1 BYTE                          
         BNE   VOPTN                                                            
         LA    R5,1(R5)             R5=NEXT FILETAB ENTRY                       
         LA    R4,3(R4)                                                         
         CLC   0(3,R4),SPACES       TEST END OF CARD                            
         BE    VOPTY                                                            
         B     VFIL3                                                            
                                                                                
VPDU     CLI   0(R4),C'N'          PDUMP=N                                      
         BNE   *+8                                                              
         OI    FILOPT,FILNPD                                                    
         B     VOPTY                                                            
*                                                                               
VOUT     CLI   0(R4),C'N'          CREATE OUTPUT FILE                           
         BE    VOPTY                                                            
         CLI   0(R4),C'Y'                                                       
         BNE   VOPTN                                                            
         OI    FILOPT,FILOUT                                                    
         B     VOPTY                                                            
                                                                                
*                                                                               
VTRM     L     R3,AIO1              TERMNUM=#####                               
         GOTO1 SCANNER,DMCB,(C'C',CARDIO),(R3)                                  
         CLI   4(R1),1                  ERROR                                   
         BNE   VOPTN                                                            
         SR    R4,R4                                                            
         ICM   R4,1,SCANRLN        TEST LENGTH OF INPUT                         
         BZ    VOPTN               NO INPUT                                     
         CH    R4,=H'5'            TEST LENGTH MORE THAN 5                      
         BH    VOPTN                                                            
         TM    SCANRST,SCANNUM     TEST NUMERIC                                 
         BNO   VOPTN                                                            
         OC    SCANRBV,SCANRBV     BINARY NUMBER                                
         BZ    VOPTN                                                            
         MVC   TERM,SCANRBV+2      SAVE TERMINAL NUMBER                         
         B     VOPTY                                                            
         EJECT                                                                  
VLTI     LA    R5,LOTIM            LOWTIME=HH:MM:SS/HH:MM                       
         B     *+8                                                              
VHTI     LA    R5,HITIM            HIGHTIME=                                    
         MVC   DBL(7),=C'0000000'                                               
         LA    RF,DBL+1                                                         
         BAS   RE,NUMT             TEST HOURS                                   
         BNE   VOPTN                                                            
         LA    R4,3(R4)                                                         
         LA    RF,2(RF)                                                         
         BAS   RE,NUMT             TEST MINUTES                                 
         BNE   VOPTN                                                            
         LA    R4,3(R4)                                                         
         LA    RF,2(RF)                                                         
         BAS   RE,NUMT             TEST SECONDS (NOT REQUIRED)                  
         PACK  0(L'LOTIM,R5),DBL(7)                                             
         B     VOPTY                                                            
*                                                                               
NUMT     CLI   0(R4),C'0'          TEST 2 NUMERIC                               
         BLR   RE                                                               
         CLI   0(R4),C'9'                                                       
         BHR   RE                                                               
         CLI   1(R4),C'0'                                                       
         BLR   RE                                                               
         CLI   1(R4),C'9'                                                       
         BHR   RE                                                               
         MVC   0(2,RF),0(R4)        SAVE CHARACTER                              
         CLI   *+1,1                                                            
         BR    RE                                                               
         EJECT                                                                  
VDSK     L     R3,AIO1              DSIKADR=XXXXXXXX                            
         GOTO1 SCANNER,DMCB,(C'C',CARDIO),(R3)                                  
         CLI   4(R1),1                  ERROR                                   
         BNE   VOPTN                                                            
         SR    R4,R4                                                            
         ICM   R4,1,SCANRLN         TEST LENGTH OF INPUT                        
         BZ    VOPTN                NO INPUT                                    
         CH    R4,=H'8'             TEST LENGTH MUST BE 8                       
         BNE   VOPTN                                                            
         GOTO1 HEXIN,DMCB,SCANRHT,DSKADR,(R4)                                   
         OC    12(4,R1),12(R1)      TEST LENGTH OF DESTINATION                  
         BZ    VOPTN                                                            
         B     VOPTY                                                            
                                                                                
*                                                                               
VUSR     L     R3,AIO1             USERID=#####                                 
         GOTO1 SCANNER,DMCB,(C'C',CARDIO),(R3)                                  
         CLI   4(R1),1             ERROR                                        
         BNE   VOPTN                                                            
         SR    R4,R4                                                            
         ICM   R4,1,SCANRLN        TEST LENGTH OF INPUT                         
         BZ    VOPTN               NO INPUT                                     
         TM    SCANRST,SCANNUM     TEST NUMERIC                                 
         BNO   VOPTN                                                            
         OC    SCANRBV,SCANRBV     BINARY NUMBER                                
         BZ    VOPTN                                                            
         MVC   USER,SCANRBV+2      SAVE USER NUMBER                             
         B     VOPTY                                                            
         EJECT                                                                  
VTYP     CLC   0(4,R4),=C'COPY'    TYPE=COPY,CHANGE,..                          
         BNE   *+16                                                             
         OI    TYPE,TYPCPY                                                      
         LA    R4,5(R4)                                                         
         B     VTYP                                                             
         CLC   0(6,R4),=C'CHANGE'  CHANGE                                       
         BNE   *+16                                                             
         OI    TYPE,TYPCHA                                                      
         LA    R4,7(R4)                                                         
         B     VTYP                                                             
         CLC   0(3,R4),=C'ADD'      ADD                                         
         BNE   *+16                                                             
         OI    TYPE,TYPADD                                                      
         LA    R4,4(R4)                                                         
         B     VTYP                                                             
         CLC   0(3,R4),SPACES      TEST END OF CARD                             
         BNE   VOPTN                                                            
         CLI   TYPE,0              TEST ANY TYPE                                
         BE    VOPTN                                                            
         B     VOPTY                                                            
*                                                                               
VOPTN    B     INITERR             ERROR EXIT                                   
*                                                                               
VOPTY    B     INIT1               OK GET NEXT CARD                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY RECORD - FROM MONACC                               *         
***********************************************************************         
                                                                                
         USING RCVRECD,R3                                                       
PRCV     L     R3,ADTRANS          A(RECOVERY RECORD)                           
         BAS   RE,FLTRCV           FILTER RECOVERY HEADER                       
         BNE   XIT                                                              
         LA    R4,RCVRECRD                                                      
         BAS   RE,FLTFIL           FILTER FILE RECORD                           
         BNE   XIT                                                              
         BAS   RE,PDMP                                                          
         BAS   RE,MAIN             WHATEVER NEEDS TO BE DONE                    
         LR    R4,R3                                                            
         BAS   RE,OUTF                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS FROM AN INPUT FILE                                  *         
***********************************************************************         
                                                                                
PFIL     TM    FILOPT,FILTOD       TEST INPUT (RECOVERY) FROM TAPE              
         BNO   PFIL3                                                            
         L     R2,ARCVTAPE                                                      
         L     R3,AIO1             R3=A(RECOVERY HEADER)                        
         USING RCVRECD,R3                                                       
         GET   (R2),(R3)                                                        
         BAS   RE,FLTRCV           FILTER RECOVERY HEADER                       
         BNE   PFIL                                                             
         LA    R4,RCVRECRD                                                      
         B     PFIL5                                                            
*                                                                               
PFIL3    TM    FILOPT,FILACC       TEST INPUT (ACCOUNT FILE) FROM TAPE          
         BNO   XIT                                                              
         L     R2,ATINT            GET ACCOUNT FILE RECORD                      
         L     R3,AIO1                                                          
         GET   (R2),(R3)                                                        
         LA    R4,4(R3)            R4=A(ACCOUNT FILE RECORD)                    
*                                                                               
PFIL5    BAS   RE,FLTFIL           FILTER FILE RECORD                           
         BNE   PFIL                                                             
         BAS   RE,PDMP                                                          
         BAS   RE,MAIN             WHATEVER NEEDS TO BE DONE                    
         L     R4,AIO1                                                          
         BAS   RE,OUTF                                                          
         B     PFIL                GET NEXT                                     
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER RECOVERY HEADER                                              *         
***********************************************************************         
                                                                                
         USING RCVRECD,R3                                                       
FLTRCV   NTR1  ,                                                                
         LA    R1,RCVFILTY         FILE NUMBER                                  
         LA    R2,FILES                                                         
         BAS   RE,TEST                                                          
         BNE   XNO                                                              
*                                                                               
         LA    R1,RCVPRGNO         PROGRAM                                      
         LA    R2,PRGMS                                                         
         BAS   RE,TEST                                                          
         BNE   XNO                                                              
*                                                                               
         OC    SIN,SIN             SYSTEM INPUT NUMBER                          
         BZ    *+14                                                             
         CLC   SIN,RCVSEQNO+1                                                   
         BNE   XNO                                                              
*                                                                               
         OC    TERM,TERM           TERMINAL NUMBER FILTER                       
         BZ    *+14                                                             
         CLC   RCVTERM,TERM                                                     
         BNE   XNO                                                              
*                                                                               
         OC    LOTIM,LOTIM         TEST LOW TIME                                
         BZ    *+14                                                             
         CP    RCVTIME,LOTIM                                                    
         BL    XNO                                                              
*                                                                               
         OC    HITIM,HITIM         TEST HIGH TIME                               
         BZ    *+14                                                             
         CP    RCVTIME,HITIM                                                    
         BH    XNO                                                              
*                                                                               
         OC    DSKADR,DSKADR       DISK ADDRESS                                 
         BZ    *+14                                                             
         CLC   RCVRECDA,DSKADR                                                  
         BNE   XNO                                                              
*                                                                               
         OC    USER,USER           USERID                                       
         BZ    *+14                                                             
         CLC   RCVUSRID,USER                                                    
         BNE   XNO                                                              
*                                                                               
         CLI   TYPE,0              ALL TYEPS                                    
         BE    FLTRCV3                                                          
         CLI   RCVRECTY,RCVRCPYQ   COPY                                         
         BNE   *+12                                                             
         TM    TYPE,TYPCPY                                                      
         BNO   XNO                                                              
         CLI   RCVRECTY,RCVRCHAQ   CHANGE                                       
         BNE   *+12                                                             
         TM    TYPE,TYPCHA                                                      
         BNO   XNO                                                              
         CLI   RCVRECTY,RCVRADDQ   ADD                                          
         BNE   *+12                                                             
         TM    TYPE,TYPADD                                                      
         BNO   XNO                                                              
*                                                                               
FLTRCV3  B     XYES                                                             
         DROP  R3                                                               
*                                                                               
TEST     CLI   0(R2),0             TEST ANY FILTER DATA                         
         BER   RE                  NO, TAKE ALL                                 
         CLC   0(1,R1),0(R2)       MATCH RECORD TO TABLE                        
         BER   RE                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),0             TEST EOT                                     
         BNE   TEST                                                             
         CLI   *,0                                                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE RECORD                                         *         
***********************************************************************         
                                                                                
         USING ACCRECD,R4                                                       
FLTFIL   NTR1  ,                                                                
*&&DO                                                                           
********* TEMP   CODE *****                                                     
         LA    R1,LDGRLST                                                       
FLTFIL1  CLC   1(2,R4),0(R1)                                                    
         BE    FLTOK                                                            
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   FLTFIL1                                                          
         B     XNO                                                              
LDGRLST  DC    C'SCSPSQSUSSST',X'FF'                                            
FLTOK    DS    0H                                                               
*******END   TEMP   CODE *****                                                  
*&&                                                                             
         SR    R1,R1                                                            
         LA    R2,KYFLN            COMPARE FILTER KEY                           
         BAS   RE,COMPK                                                         
         BNE   XNO                                                              
         LA    R2,LOWKLN           COMPARE LOW KEY                              
         BAS   RE,COMPK                                                         
         BL    XNO                                                              
         LA    R2,HIKLN            COMPARE HIGH KEY                             
         BAS   RE,COMPK                                                         
         BH    XNO                                                              
         B     XYES                                                             
*                                                                               
COMPK    ICM   R1,1,0(R2)          TEST FILTER KEY                              
         BZR   RE                  NO, FILTER TAKE ALL                          
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         CLC   ACCKEY(0),1(R2)     COMPARE KEY                                  
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PDUMP OF INPUT RECORDS                                              *         
***********************************************************************         
                                                                                
PDMP     TM    FILOPT,FILNPD       SUPPRESS PDUMPS                              
         BOR   RE                                                               
PDMP1    NTR1  ,                                                                
         MVI   LINE,1                                                           
         TM    FILOPT,FILACC       IS IT AN ACCOUNT FILE                        
         BO    PDMP3               SKIP RECOVERY HEADER                         
*                                                                               
         USING RCVRECD,R3                                                       
         LA    R5,P                                                             
         MVC   0(5,R5),=C'FILE='      FILE=                                     
         GOTO1 HEXOUT,DMCB,RCVFILTY,5(R5),1                                     
         LA    R5,8(R5)                                                         
*                                                                               
         MVC   0(5,R5),=C'TYPE='    TYPE=                                       
         CLI   RCVRECTY,RCVRCPYQ     COPY?                                      
         BNE   *+10                                                             
         MVC   5(4,R5),=C'COPY'                                                 
         CLI   RCVRECTY,RCVRCHAQ     CHANGE?                                    
         BNE   *+10                                                             
         MVC   5(3,R5),=C'CHG'                                                  
         CLI   RCVRECTY,RCVRADDQ     ADD?                                       
         BNE   *+10                                                             
         MVC   5(3,R5),=C'ADD'                                                  
         LA    R5,10(R5)                                                        
*                                                                               
         MVC   0(6,R5),=C'TERM#='  TERM=                                        
         XC    DBL,DBL                                                          
         MVC   DBL+1(2),RCVTERM                                                 
         EDIT  (B3,DBL),(5,6(R5)),ALIGN=LEFT                                    
         LA    R5,13(R5)                                                        
*                                                                               
         MVC   0(4,R5),=C'SIN='           SIN=                                  
         GOTO1 HEXOUT,DMCB,RCVSEQNO,4(R5),4,0                                   
         LA    R5,14(R5)                                                        
*                                                                               
         GOTO1 HEXOUT,DMCB,RCVTIME,DBL,4  TIME=                                 
         MVC   0(5,R5),=C'TIME='                                                
         MVC   5(2,R5),DBL+1                                                    
         MVI   7(R5),C':'                                                       
         MVC   8(2,R5),DBL+3                                                    
         MVI   10(R5),C':'                                                      
         MVC   11(2,R5),DBL+5                                                   
         LA    R5,14(R5)                                                        
*                                                                               
         MVC   0(7,R5),=C'DSKADD='         DSKADD=                              
         GOTO1 HEXOUT,DMCB,RCVRECDA,7(R5),4                                     
         LA    R5,16(R5)                                                        
*                                                                               
         MVC   0(5,R5),=C'DATE='                                                
         GOTO1 DATCON,DMCB,(3,RCVDATE),(11,5(R5))                               
         LA    R5,14(R5)                                                        
*                                                                               
         LA    R5,PSECOND                                                       
         MVC   0(5,R5),=C'USER='                                                
         EDIT  RCVUSRID,(4,5(R5)),ALIGN=LEFT                                    
         LA    R5,11(R5)                                                        
*                                                                               
         MVC   0(4,R5),=C'PGM='    PROGRAM=                                     
         LA    RF,PGMTAB                                                        
         CLC   RCVPRGNO,0(RF)                                                   
         BE    *+20                                                             
         LA    RF,L'PGMTAB(RF)                                                  
         CLI   0(RF),X'FF'         TEST EOT                                     
         BNE   *-18                                                             
         B     *+14                                                             
         MVC   4(11,R5),1(RF)                                                   
         LA    R5,16(R5)                                                        
*                                                                               
         GOTO1 ADSQUASH,DMCB,P,132                                              
         GOTO1 ADSQUASH,DMCB,PSECOND,132                                        
         GOTO1 ACREPORT                                                         
*                                                                               
PDMP3    SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(RF),=C'2D',0                         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE OUTPUT FILE                                                  *         
***********************************************************************         
                                                                                
OUTF     TM    FILOPT,FILOUT                                                    
         BNOR  RE                                                               
         ST    RE,SAVRE                                                         
         PUT   TOUT,(R4)                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIAL PROCESS ROUTINE                                             *         
*   R4 = A(ACCOUNT FILE RECORD)                                       *         
***********************************************************************         
                                                                                
MAIN     BR    RE                                                               
         NTR1  ,                                                                
         GOTO1 ACRECTYP,DMCB,(C'D',0(R4))   GET RECORD TYPE                     
         MVC   COMPDSP,2(R1)       SAVE DISPLACEMENT TO COMPANY                 
         MVC   RECTYP,0(R1)        RECORD TYPE                                  
         MVC   CURCOMP,1(R1)       CURRENT COMPANY                              
*                                                                               
         CLI   RECTYP,ACRTTRN                                                   
         BNE   XIT                                                              
         USING TRNRECD,R4                                                       
         LA    R5,TRNRFST                                                       
         USING TRNELD,R5                                                        
         CLI   TRNTYPE,7                                                        
         BNE   XIT                                                              
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R4)                                         
         GOTO1 PRNTBL,DMCB,0,(R4),C'DUMP',(RF),=C'2D',(C'P',PRINT)              
         B     XIT                                                              
*                                                                               
         LA    R5,DKEY                                                          
         USING ORDRECD,R5                                                       
         MVC   ORDKEY,0(R4)                                                     
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         CLI   DMCB+8,0            TEST ON FILE                                 
         BE    XNO                 GOOD, GET ANOTHER                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,ORDRLEN-ORDRECD(R4)                                         
         GOTO1 PRNTBL,DMCB,0,(R4),C'DUMP',(RF),=C'2D',(C'P',PRINT)              
*                                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R4),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   XIT                                                              
         BASR  RE,RF               GO TO DATAMGR                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XYES                                                             
*                                                                               
         LA    R4,DIR                                                           
         MVC   DA,ORDKDA                                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO2,DMWORK                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PRNTBL,DMCB,0,(R4),C'DUMP',(R2),=C'2D',(C'P',PRINT)              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET COMPANY RECORD                                                  *         
***********************************************************************         
                                                                                
GETCPY   NTR1  ,                                                                
         LA    R4,DKEY             GET COMPANY RECORD TO ESTABLISH DA           
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,DIR                                                           
         MVC   DA,CPYKDA                                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO2,DMWORK                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
XYES     CR    RB,RB                                                            
         B     *+6                                                              
XNO      LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CLOSE FILES                                                         *         
***********************************************************************         
                                                                                
CLOSE    L     R2,ARCVTAPE                                                      
         TM    FILOPT,FILTOD       TEST INPUT (RECOVERY) FROM TAPE              
         BO    *+8                                                              
         L     R2,ATINT            GET ACCOUNT FILE RECORD                      
         CLOSE ((R2))              CLOSE INPUT FILE                             
         NI    FILOPT,X'FF'-(FILTOD)                                            
*                                                                               
         TM    FILOPT,FILOUT       TEST OUTPUT FILE                             
         BNO   XIT                                                              
         CLOSE (TOUT)                                                           
         NI    FILOPT,X'FF'-(FILOUT)                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCFIL   DC    C'ACCOUNT '                                                      
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
READ     DC    C'DMREAD  '                                                      
*                                                                               
ACRECTYP DC    V(ACRECTYP)                                                      
DECODE   DC    V(DECODE)                                                        
HELLO    DC    V(HELLO)                                                         
HEXIN    DC    V(HEXIN)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SCANNER  DC    V(SCANNER)                                                       
*                                                                               
ARCVTAPE DC    A(RCVTAPE)                                                       
ATINT    DC    A(TINT)                                                          
ATOUT    DC    A(TOUT)                                                          
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
         EJECT                                                                  
***********************************************************************         
* PROGRAM TABLE                                                       *         
***********************************************************************         
                                                                                
PGMTAB   DS    0XL12                                                            
         DC    AL1(RCVPPFMQ),CL11'PFM        '                                  
         DC    AL1(RCVPBATQ),CL11'BATCH      '                                  
         DC    AL1(RCVPFILQ),CL11'FILE       '                                  
         DC    AL1(RCVPREQQ),CL11'REQUEST    '                                  
         DC    AL1(RCVPAISQ),CL11'AIS        '                                  
         DC    AL1(RCVPINFQ),CL11'INF0       '                                  
         DC    AL1(RCVPCSHQ),CL11'CASH       '                                  
         DC    AL1(RCVPRCVQ),CL11'RCV        '                                  
         DC    AL1(RCVPVOIQ),CL11'VOID       '                                  
         DC    AL1(RCVPPROQ),CL11'PROD       '                                  
         DC    AL1(RCVPSCRQ),CL11'SCRIBE     '                                  
         DC    AL1(RCVPCRDQ),CL11'CRD        '                                  
         DC    AL1(RCVPBILQ),CL11'BILL       '                                  
         DC    AL1(RCVPORDQ),CL11'ORDERS     '                                  
         DC    AL1(RCVPBUDQ),CL11'BUDGET     '                                  
         DC    AL1(RCVPMACQ),CL11'MAC        '                                  
         DC    AL1(RCVPREGQ),CL11'REGISTER   '                                  
         DC    AL1(RCVPINVQ),CL11'INVOICE    '                                  
         DC    AL1(RCVPWRIQ),CL11'WRITER     '                                  
         DC    AL1(RCVPEXPQ),CL11'EXPEND     '                                  
         DC    AL1(RCVPMRKQ),CL11'MARKER     '                                  
         DC    AL1(RCVPAWKQ),CL11'ACWK       '                                  
         DC    AL1(RCVPCLOQ),CL11'CLO        '                                  
         DC    AL1(RCVPINTQ),CL11'INTERAGENCY'                                  
         DC    AL1(RCVPWFMQ),CL11'WFM        '                                  
         DC    AL1(RCVPINPQ),CL11'INPUT      '                                  
         DC    AL1(RCVPCAPQ),CL11'CAP        '                                  
         DC    AL1(RCVPCTAQ),CL11'CTA        '                                  
         DC    AL1(RCVPTAPQ),CL11'TAP        '                                  
         DC    AL1(RCVPFISQ),CL11'FIS        '                                  
         DC    AL1(RCVPCBLQ),CL11'CBILL      '                                  
         DC    AL1(RCVPTRNQ),CL11'TRANSFER   '                                  
         DC    AL1(RCVPRFPQ),CL11'RFP        '                                  
         DC    AL1(RCVPTEMQ),CL11'TEMPO      '                                  
         DC    AL1(RCVPPREQ),CL11'PRESTO     '                                  
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,MACRF=GM,EODAD=CLOSE,           X        
               RECFM=VB,LRECL=8200                                              
*                                                                               
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=CLOSE,            X        
               RECFM=VB,LRECL=2048                                              
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=2048                                              
         EJECT                                                                  
***********************************************************************         
*              STORAGE                                                *         
***********************************************************************         
*                                                                               
IO1      DS    8204C                                                            
*                                                                               
IO2      DS    8204C                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PROGRAM STORAGE                                           *         
***********************************************************************         
*                                                                               
ACXTD    DSECT                                                                  
DBL      DS    D                                                                
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    XL4                                                              
SAVRE    DS    F                                                                
*                                                                               
COMPDSP  DS    XL1                 DISPLACEMENT TO COMPANY CODE                 
RECTYP   DS    XL1                 RECORD TYPE                                  
CURCOMP  DS    XL1                 COMPANY CODE                                 
*                                                                               
CARDIO   DS    CL80                                                             
*                                                                               
FILOPT   DS    XL1                 FILE OPTIONS                                 
FILRCV   EQU   X'80'               INPUT FILE IS ACCRCV (ONLINE)                
FILTOD   EQU   X'40'               TAPE OR DISK RECOVERY                        
FILACC   EQU   X'20'               ACCOUNT FILE TAPE                            
FILOUT   EQU   X'08'               CREATE OUTPUT FILE                           
FILNPD   EQU   X'04'               NO PDUMPS                                    
*                                                                               
KYFLN    DS    XL1                 KEY FILTER LENGTH                            
KYF      DS    XL(L'ACCKEY)        KEY FILTER                                   
LOWKLN   DS    XL1                 LOW KEY LENGTH                               
LOWK     DS    XL(L'ACCKEY)        LOW FILTER                                   
HIKLN    DS    XL1                 HIGH KEY LENGTH                              
HIK      DS    XL(L'ACCKEY)        HIGH FILTER                                  
*                                                                               
SIN      DS    XL3                 SYSTEM INPUT NUMBER                          
FILES    DS    XL10                FILE NUMBERS 69,6A ETC.                      
PRGMS    DS    XL10                PROGRAM NUMBERS 0E,12 ETC.                   
TERM     DS    XL2                 TERMINAL NUMBER                              
LOTIM    DS    PL4                 LOW TIME                                     
HITIM    DS    PL4                 HIGH TIME                                    
DSKADR   DS    XL4                 DISK ADDRESS                                 
USER     DS    XL2                 USER ID                                      
TYPE     DS    XL1                 RECORD TYPE                                  
TYPCPY   EQU   X'80'               COPY                                         
TYPCHA   EQU   X'40'               CHANGE                                       
TYPADD   EQU   X'20'               ADD                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR SCAN BLOCK                                                *         
***********************************************************************         
*                                                                               
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
SCANRHT  DS    CL20                RIGHT DATA                                   
SCANLNQ  EQU   *-SCAND                                                          
         EJECT                                                                  
       ++INCLUDE ACRCVRECD                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT  ON                                                              
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT  ON                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPXT02 07/28/03'                                      
         END                                                                    
