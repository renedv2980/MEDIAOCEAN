*          DATA SET DDTWATRC   AT LEVEL 083 AS OF 03/23/15                      
*PHASE TWATRCA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE TWANG                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE FATABOFF                                                               
         SPACE 2                                                                
         TITLE 'PRINT TRACED SCREENS'                                           
         PRINT NOGEN                                                            
TWATRC   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKX-WORKD,*TWATRC*,=V(REGSAVE),RA,CLEAR=YES                    
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(TBLOCK-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,ATBLOCK                                                       
         L     R1,=A(ADRBUFF-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AADRBUFF                                                      
*                                                                               
         BAS   RE,INIT             INIT + READ CARDS                            
         BAS   RE,OPENFLS          OPEN FILES                                   
         BAS   RE,MAIN                                                          
         BAS   RE,CLOSE            CLOSE FILES                                  
XBASE    XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         L     R3,AADRBUFF                                                      
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT050                                                          
         MVC   PRL(80),0(R3)                                                    
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   ERR1                NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
INIT050  CLI   INPUT,C'T'          TEST INPUT=TAPE                              
         BE    *+12                                                             
         CLI   INPUT,C'D'          TEST INPUT=DISK                              
         BNE   ERR2                INVALID PARAMETER                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        OPEN FILES                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENFLS  NTR1                                                                   
         CLI   INPUT,C'D'          OPEN ADRFILE FOR INPUT                       
         BNE   OPEN020                                                          
         L     R3,AADRBUFF                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,SERVICE,FILELIST,(R3)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPEN020  CLI   INPUT,C'T'          OPEN ADRTAPE FOR INPUT                       
         BNE   OPEN030                                                          
         OPEN  ADRIN                                                            
*                                  INITIALISE SORTER                            
OPEN030  GOTO1 =V(SORTER),DMCB,SCARD1,SCARD2                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         L     R3,AADRBUFF         R3=A(BUFFER)                                 
*                                                                               
MAIN010  BAS   RE,ADRGET           GET AN ADR RECORD                            
*                                                                               
         CLI   0(R3),X'FF'         TEST FOR EOF                                 
         BE    MAIN030                                                          
         CLC   0(4,R3),=C'**TW'    TEST FOR **TWA                               
         BE    MAIN020                                                          
         CLI   0(R3),C'*'          IGNORE OTHER SPECIALS                        
         BE    MAIN010                                                          
*                                                                               
         B     MAIN010             IGNORE NORMAL ADR RECS                       
*                                                                               
MAIN015  LA    R0,8                CHECH LUID FILTER                            
         LA    R1,FLUID                                                         
         LA    RF,16(R3)                                                        
MAIN016  CLI   0(R1),C'*'                                                       
         BE    MAIN020                                                          
         CLC   0(1,R1),0(RF)                                                    
         BNE   MAIN010                                                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,MAIN016                                                       
*                                                                               
MAIN020  LA    R2,ADRHDR           PUT TIME INTO ADRHDR AND SORT                
         MVC   0(4,R2),ADRSTTM-ADRREC+24(R2)                                    
         GOTO1 =V(SORTER),DMCB,PUT,(R2)                                         
         B     MAIN010                                                          
*                                                                               
MAIN030  LA    R1,TITLE2           PRINT TWA TITLE                              
         BAS   RE,PRINTT                                                        
*                                                                               
MAIN040  GOTO1 =V(SORTER),DMCB,GET GET TWAS BACK FROM SORT                      
         ICM   R3,15,4(R1)                                                      
         BZ    MAIN090             END OF FILE                                  
*                                                                               
         SR    R1,R1               PAGE EJECT ON NEW TWA                        
         BAS   RE,PRINTT                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,24(R3)           OUTPUT TRANSACTION DETAIL                    
         LR    R1,R2                                                            
         BAS   RE,TRANSOUT                                                      
*                                                                               
         CLC   9(2,R2),=X'0105'    TEST FOR SRTOP                               
         BNE   MAIN060                                                          
         LA    R2,48(R2)                                                        
         GOTO1 =V(PRNTBL),DMCB,(8,TWAT),(R2),C'DUMP',1900,=C'1D  ',0            
         BAS   RE,PRINTL                                                        
         B     MAIN040                                                          
*                                                                               
MAIN060  LA    R2,48(R2)                                                        
         GOTO1 =V(TWANG),DMCB,(R2),ATBLOCK                                      
*                                                                               
         MVC   PRL(82),BOXTOP      BOX IT                                       
         BAS   RE,PRINTL                                                        
         L     R2,ATBLOCK                                                       
         LA    R0,24                                                            
MAIN070  MVI   PRL+00,X'FA'        BOX IT                                       
         MVC   PRL+1(80),0(R2)     PRINT TWA LINE                               
         MVI   PRL+81,X'FA'        BOX IT                                       
         BAS   RE,PRINTL                                                        
         LA    R2,80(R2)                                                        
         BCT   R0,MAIN070                                                       
         MVC   PRL(82),BOXBOT      BOX IT                                       
         BAS   RE,PRINTL                                                        
         B     MAIN040             BACK FOR NEXT                                
*                                                                               
MAIN090  GOTO1 =V(SORTER),DMCB,END                                              
*                                                                               
MAINXX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE                                              *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         CLI   INPUT,C'T'          CLOSE TAPEIN IF USED                         
         BNE   CLOSEX                                                           
         CLOSE ADRIN                                                            
CLOSEX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        ROUTINES FOR READING ADR RECORDS                   *                   
*************************************************************                   
         SPACE 1                                                                
ADRGET   NTR1                                                                   
         L     R3,AADRBUFF                                                      
*                                                                               
         CLI   INPUT,C'D'          TEST DISK INPUT                              
         BNE   AGET020                                                          
         OC    ADRADR,ADRADR       TEST FIRST TIME                              
         BNZ   *+10                                                             
         MVC   ADRADR,=X'00010100'                                              
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,ADRFILE,ADRADR,(R3)                      
         TM    8(R1),X'80'                                                      
         BO    ADREND              END-OF-FILE                                  
         CLI   8(R1),0                                                          
         BE    AGET050             GOT A RECORD                                 
         DC    H'0'                                                             
*                                                                               
AGET020  CLI   INPUT,C'T'          TEST TAPE INPUT                              
         BNE   AGET030                                                          
*                                                                               
         GET   ADRIN,(R3)          GET TAPE RECORD                              
         B     AGET050                                                          
*                                                                               
AGET030  DC    H'0'                INPUT=(UNKNOWN)                              
*                                                                               
AGET050  B     EXIT                EXIT REC IN ADRBUFF                          
*                                                                               
ADREND   MVC   0(16,R3),FFS        EXIT FFS IN ADRBUFF                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT TRANSACTION DETAIL                           *                   
*************************************************************                   
         SPACE 1                                                                
TRANSOUT NTR1                                                                   
         LR    R2,R1                                                            
         MVC   PRL(8),0(R2)                                                     
*                                                                               
         MVC   GTSYSSE,9(R2)       PRINT SYSTEM NAME                            
         BAS   RE,GETSYS                                                        
         MVC   PRL+10(3),GTSYSN                                                 
*                                                                               
         MVC   GTPROG,10(R2)       PRINT PROG NAME                              
         BAS   RE,GETPROG                                                       
         MVC   PRL+24(7),GTPROGN                                                
*                                                                               
         MVC   FULL,16(R2)         END TIME                                     
         L     RF,20(R2)                                                        
         S     RF,FULL             RF=ELAPSED TIME                              
         SR    RE,RE                                                            
         D     RE,=F'6000'         RF=MINS RE=SECS*100                          
*                                                                               
         MVC   PRL+34(3),=C'ET='                                                
         EDIT  (RF),(3,PRL+37),ZERO=NOBLANK,TRAIL=C'm'                          
         EDIT  (RE),(6,PRL+41),2,ZERO=NOBLANK,TRAIL=C's'                        
*                                                                               
         MVC   PRL+48(4),=C'from'                                               
*                                                                               
         MVC   FULL,16(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+53(8),WORK1                                                  
         MVC   PRL+62(2),=C'to'                                                 
         MVC   FULL,20(R2)                                                      
         BAS   RE,TIMEOUT                                                       
         MVC   PRL+65(8),WORK1                                                  
*                                                                               
         MVC   PRL+76(4),=C'CPU='                                               
         EDIT  (B4,24(R2)),(6,PRL+80),2,ALIGN=LEFT                              
         MVC   PRL+88(4),=C'I/O='                                               
         EDIT  (B2,34(R2)),(6,PRL+92),ALIGN=LEFT                                
*                                                                               
         MVC   PRL+99(4),=C'SIN='                                               
         GOTO1 =V(HEXOUT),DMCB,12(R2),PRL+103,4                                 
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINTER ROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
PRINTT   ST    RE,SAVERE           SET TITLES AND FORCE PAGE THROW              
         LTR   RF,R1                                                            
         L     R1,=V(CPRINT)                                                    
         USING DPRINT,R1                                                        
         BZ    PRINTT1             IF R1 WAS ZERO JUST FORCE NEW PAGE           
         MVC   TITLE,SPACES        CLEAR TITLE TO SPACES                        
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              EX TITLES INTO TITLE                         
         B     *+10                                                             
         MVC   TITLE(0),1(RF)                                                   
PRINTT1  ZAP   LINE,=P'99'         ZAP LINE TO FORCE NEW PAGE                   
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         SPACE 1                                                                
PRINTL   ST    RE,SAVERE           PRINT A LINE                                 
         L     R1,=V(CPRINT)                                                    
         MVC   P,PRL                                                            
         GOTO1 =V(PRINTER)         GOTO PRINTER                                 
         MVC   PRL,SPACES1                                                      
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         DROP  R1                                                               
         EJECT                                                                  
*************************************************************                   
*        MISC SUBROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
GTOVSYS  ST    RE,SAVERE           GET SYS NAME INTO WORK                       
         SR    R1,R1                                                            
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   0(1,R1),BYTE        SYSTEM NUMBER IN BYTE                        
         BE    GTOV010                                                          
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GTOV010  MVC   WORK(8),9(R1)       OV SYSTEM                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GTSESYS  ST    RE,SAVERE           GET SE NAME INTO WORK                        
         SR    R1,R1               AND A(PGMS) TAB INTO FULL                    
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   7(1,R1),BYTE        SE NUMBER IN BYTE                            
         BE    GTSE010                                                          
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GTSE010  MVC   WORK(8),0(R1)       SE SYSTEM                                    
         MVC   FULL,24(R1)         A(PROG TABLE)                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***************************************************                             
*        TIME EDIT SUBROUTINE FULL TO WORK1       *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(13),=C'00:00:00   00'                                      
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'384'                                                       
         SR    RE,RE                                                            
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+11),FILL=0    100/SEC                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ALLOCATE STORAGE AREA                              *                   
*        ENTRY R1=LENGTH OF AREA IN K.                      *                   
*************************************************************                   
         SPACE 1                                                                
GETMAIN  ST    RE,SAVERE                                                        
*                                                                               
         SLL   R1,10               MULTIPLY R1 BY 1K                            
         LR    R0,R1                                                            
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY)                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ERRORS                                             *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   PRL(32),=C'PARAMETER CARD - INVALID KEYWORD'                     
         B     ERRX                                                             
ERR2     MVC   PRL(34),=C'PARAMETER CARD - INVALID PARAMETER'                   
         B     ERRX                                                             
ERRX     L     RD,SAVERD                                                        
         BAS   RE,PRINTL                                                        
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4,0),X'0000',AL3(INPUT)                           
         DC    C'LUID   ',AL1(3,8),X'4000',AL3(FLUID)                           
         DC    C'SYSTEM ',AL1(5,0),X'C000',AL3(VALOVS)                          
         DC    C'SESYS  ',AL1(4,0),X'C000',AL3(VALSYS)                          
         DC    C'PROGRAM',AL1(6,0),X'C000',AL3(VALPROG)                         
         DC    C'TASK   ',AL1(3,0),X'6000',AL3(FTASKC)                          
         DC    C'SIN    ',AL1(2,4),X'7400',AL3(FSINC)                           
         DC    C'STIME  ',AL1(4,4),X'6600',AL3(FSTIME)                          
         DC    C'ETIME  ',AL1(4,4),X'6600',AL3(FETIME)                          
         DC    C'CPUTIME',AL1(6,0),X'6800',AL3(FCPUC)                           
         DC    C'QTIME  ',AL1(4,0),X'6800',AL3(FQTMC)                           
         DC    C'OICOUNT',AL1(6,0),X'6800',AL3(FIOCC)                           
         DC    C'OVCOUNT',AL1(6,0),X'6800',AL3(FOVCC)                           
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
INPUT    DC    C'T'                INPUT=TAPE                                   
*                                                                               
FTASKC   DC    X'00'                                                            
FTASK    DC    C' '                TASK FILTER                                  
FIOCC    DC    X'00'                                                            
FIOC     DC    XL2'0000'           I/O COUNT FILTER                             
FOVCC    DC    X'00'                                                            
FOVC     DC    XL2'0000'           OV COUNT FILTER                              
FQTMC    DC    X'00'                                                            
FQTM     DC    XL2'0000'           QUEUE TIME                                   
FCPUC    DC    X'00'                                                            
FCPU     DC    XL2'0000'           CPU TIME                                     
*                                                                               
FSESYS   DC    X'00'                                                            
FOVSYS   DC    X'00'                                                            
FPROG    DC    X'00'                                                            
*                                                                               
FLUID    DC    C'*       '                                                      
*                                                                               
FSINC    DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
         DC    X'FF'                                                            
*                                                                               
FSTIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
         DC    X'FF'                                                            
FETIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PRL+1                                                         
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET GTSYS & GTSYSN FROM GTSYSSE                    *                   
*************************************************************                   
         SPACE 1                                                                
GETSYS   NTR1                                                                   
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R4                                                       
GETSYS0  CLI   GTSYSSE,0           IF SYSSE IS ZERO                             
         BNE   GETSYS1                                                          
         CLC   GTSYS,SEOVSYS       TEST GTSYS WITH OVSYS                        
         BE    GETSYS2                                                          
GETSYS1  CLC   GTSYSSE,SESYS       TEST SE NUMBER                               
         BE    GETSYS2                                                          
         BXLE  R4,R0,GETSYS0       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     EXITNEQ             ERROR EXIT NOT FOUND                         
*                                                                               
GETSYS2  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSN,SENAME       SET NAME                                     
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     EXITEQU                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET FSESYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
         SPACE 1                                                                
VALSYS   NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         B     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  MVC   FSESYS,SESYS        SET NUMBER                                   
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     EXITEQU                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTPROGN FROM GTPROG                            *                   
*************************************************************                   
         SPACE 1                                                                
GETPROG  NTR1                                                                   
         ICM   R4,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    GETPRG2                                                          
         BAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R4                                                       
GETPRG0  CLC   GTPROG,PGMNUM       TEST PROG NUMBER                             
         BE    GETPRG1                                                          
         BXLE  R4,R0,GETPRG0       NEXT                                         
GETPRG2  XC    GTPROGN,GTPROGN     NOT FOUND SO HEXOUT NUMBER                   
         GOTO1 =V(HEXOUT),DMCB,GTPROG,GTPROGN,1                                 
         B     EXITEQU                                                          
GETPRG1  MVC   GTPROGN,PGMNAME     SET NAME                                     
         B     EXITEQU                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET FPROG FROM 0(R2) (R1)=EX LEN                   *                   
*************************************************************                   
         SPACE 1                                                                
VALPROG  NTR1                                                                   
         LR    RF,R1                                                            
         ICM   R4,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    CERRSYS                                                          
         BAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R4                                                       
VALPRG0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),PGMNAME     TEST NAME                                    
         BE    VALPRG1                                                          
         BXLE  R4,R0,VALPRG0       NEXT                                         
*                                                                               
         GOTO1 =V(HEXIN),DMCB,(R2),FPROG,2                                      
         OC    12(4,R1),12(R1)     TRY FOR HEX LAST                             
         BNZ   EXITEQU                                                          
         B     CERRPRG                                                          
*                                                                               
VALPRG1  MVC   FPROG,PGMNUM        FOUND                                        
         B     EXITEQU                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALD                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET FOVSYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
         SPACE 1                                                                
VALOVS   NTR1                                                                   
         LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
*                                                                               
VALS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALS050                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),0(R2)   TEST SHORT NAME                              
         BE    VALS990                                                          
VALS012  LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALS010                                                          
*                                                                               
VALS050  LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
VALS051  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALS090                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),0(R2)   TEST LONG NAME                               
         BE    VALS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALS051                                                          
*                                                                               
VALS090  B     CERRSYS             SET CC NEQ NOT FOUND                         
VALS990  MVC   FOVSYS,SYSLNUM                                                   
         MVC   GTSYSSE,FOVSYS                                                   
         BAS   RE,GETSYS           GET A(PGMS)                                  
         B     EXITEQU             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM NAME FROM GTSYS                         *                   
*************************************************************                   
         SPACE 1                                                                
GETOVS   NTR1                                                                   
         LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
*                                                                               
GETS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    GETS090                                                          
         CLC   GTSYS,SYSLNUM       TEST SYSTEM NUMBER                           
         BE    GETS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         B     GETS010                                                          
*                                                                               
GETS090  B     EXITNEQ             SET CC NEQ NOT FOUND                         
*                                                                               
GETS990  MVC   GTSYSN,SYSLSHRT                                                  
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         B     EXITEQU             SET CC EQU                                   
         DROP  RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
TITLE1   DC    AL1(L'TITLE1T)                                                   
TITLE1T  DC    C'PARAMETER CARDS'                                               
TITLE2   DC    AL1(L'TITLE2T)                                                   
TITLE2T  DC    C'CAPTURED SCREEN PRINTS   '                                     
*                                                                               
BOXTOP   DC    X'AC',80X'BF',X'BC'                                              
BOXBOT   DC    X'AB',80X'BF',X'BB'                                              
*                                                                               
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ADREND,          X        
               RECFM=FB,BLKSIZE=0                                               
*                                                                               
SCARD1   DC    C'SORT FIELDS=(1,8,BI,A) '                                       
SCARD2   DC    C'RECORD TYPE=F,LENGTH=6400 '                                    
GET      DC    C'GET '                                                          
PUT      DC    C'PUT '                                                          
END      DC    C'END '                                                          
*                                                                               
SPACES1  DC    CL132' '                                                         
FFS      DC    16X'FF'                                                          
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADRFILE  DC    CL8'ADRFILE'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
STATS    DC    CL8'STATS'                                                       
TWAT     DC    CL8'TWA DUMP'                                                    
*                                                                               
SERVICE  DC    CL8'SERVICE'                                                     
FILELIST DC    C'NADRFIL X'                                                     
*                                                                               
       ++INCLUDE FASYSLST                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
TEMP     DS    CL80                                                             
WORK     DS    CL80                                                             
WORK1    DS    CL80                                                             
PRL      DS    CL132                                                            
*                                                                               
ADRADR   DS    F                                                                
SAVERE   DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
ATBLOCK  DS    A                                                                
AADRBUFF DS    A                                                                
ABUFFER  DS    A                                                                
*                                                                               
CARDEND  DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
GTAPGMS  DS    A                                                                
GTPROG   DS    X                                                                
GTPROGN  DS    CL8                                                              
GTSYS    DS    X                                                                
GTSYSN   DS    CL8                                                              
GTSYSSE  DS    X                                                                
*                                                                               
ADRHDR   DS    CL8                 SORT HEADER                                  
ADRBUFF  DS    6400C               ADR BLOCK                                    
TBLOCK   DS    0D                                                               
         DS    6400C                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
SSB      CSECT                                                                  
         DS    0D                                                               
         DC    X'0000FF02'                                                      
         DC    XL256'00'                                                        
         SPACE 2                                                                
UTL      CSECT                                                                  
         DS    0D                                                               
         DC    F'0',X'01'                                                       
         EJECT                                                                  
       ++INCLUDE DDSTATREC                                                      
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083DDTWATRC  03/23/15'                                      
         END                                                                    
