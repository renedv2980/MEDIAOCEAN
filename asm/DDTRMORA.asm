*          DATA SET DDTRMORA   AT LEVEL 163 AS OF 05/01/02                      
*PHASE DDTRACEA,*                                                               
*INCLUDE FATABOFF                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE SORTER                                                                 
         SPACE 1                                                                
         TITLE 'DDTTRACE - TRACE ONLINE ACTIVITY'                               
         PRINT NOGEN                                                            
         EJECT                                                                  
TRACE    CSECT                                                                  
         NBASE WORKX-WORKD,XXTRACEX,R9,WORK=V(REGSAVE),CLEAR=YES                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,OPENADR          OPEN ADR SOURCE                              
         BAS   RE,MAIN             MAIN LOOP                                    
*                                                                               
XBASE    L     RD,SAVERD           GET HERE FROM ANYWHERE                       
         XBASE 1                                                                
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,ADRBUFF                                                       
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    EXIT                                                             
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        OPEN INPUT                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENADR  NTR1                                                                   
         OPEN  (ADRIN,INPUT)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         L     R8,=F'1000000'                                                   
*                                                                               
MAIN010  MVC   PLINE,SPACES                                                     
         GET   ADRIN                                                            
         LR    R6,R1                                                            
         CLI   0(R6),C'$'                                                       
         BE    MAIN010                                                          
*                                                                               
         BAS   RE,FILTER                                                        
         BNE   MAIN010                                                          
         BAS   RE,PRINT                                                         
         BAS   RE,PRINTL                                                        
         B     MAIN010                                                          
*                                                                               
ADREND   B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        FILTER                                                 *               
*****************************************************************               
         SPACE 1                                                                
FILTER   NTR1                                                                   
*                                                                               
         USING ADRRECD,R6                                                       
*                                                                               
         CLI   FOVSYS,0            OV SYSTEM FILTER                             
         BE    *+14                                                             
         CLC   ADROVSYS,FOVSYS                                                  
         BNE   EXITNEQ                                                          
*                                                                               
         CLI   FSESYS,0            SE SYSTEM FILTER                             
         BE    *+14                                                             
         CLC   ADRSYSNO,FSESYS                                                  
         BNE   EXITNEQ                                                          
*                                                                               
         CLI   FPROG,0             PROGRAM FILTER                               
         BE    *+14                                                             
         CLC   ADRPRGNO,FPROG                                                   
         BNE   EXITNEQ                                                          
*                                                                               
         LA    R1,FLUIDC           LUID FILTERS                                 
         CLI   0(R1),0                                                          
         BE    FILT020                                                          
FILT010  CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    EXITNEQ                                                          
         CLI   0(R1),0             TEST FOR EOT                                 
         BE    EXITNEQ                                                          
*                                                                               
FILT011  LA    RF,8(R1)            CLUID                                        
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8              COMPARE FOR ENTRY LEN-1                      
         B     *+10                                                             
         CLC   ADRSYM(0),1(R1)                                                  
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FILT020                                                        
         LA    R1,9(R1)                                                         
         B     FILT010                                                          
*                                                                               
FILT020  CLI   FTASKC,0                                                         
         BE    FILT030                                                          
         CLC   ADRTASK,FTASK                                                    
         IC    R1,FTASKC                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT030                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT030  CLI   FIOCC,0                                                          
         BE    FILT050                                                          
         CLC   ADRIOCNT,FIOC+1                                                  
         IC    R1,FIOCC                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT050                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT050  CLI   FOVCC,0                                                          
         BE    FILT060                                                          
         CLC   ADROVCNT,FOVC+3                                                  
         IC    R1,FOVCC                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT060                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT060  CLI   FQTMC,0                                                          
         BE    FILT070                                                          
         MVC   FULL,ADRINTM                                                     
         ICM   R1,15,ADRSTTM                                                    
         S     R1,FULL                                                          
         CH    R1,FQTM                                                          
         IC    R1,FQTMC                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT070                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT070  CLI   FCPUC,0                                                          
         BE    FILT080                                                          
         CLC   ADRCPUTM,FCPU                                                    
         IC    R1,FCPUC                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT080                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT080  LA    R1,FSINC                                                         
         CLI   0(R1),0                                                          
         BE    FILT090                                                          
FILT081  CLI   0(R1),0                                                          
         BE    FILT090                                                          
         CLI   0(R1),X'FF'                                                      
         BE    FILT090                                                          
         CLC   1(4,R1),ADRSIN                                                   
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         B     FILT081                                                          
*                                                                               
FILT090  LA    R1,FSTIME                                                        
         CLI   0(R1),0                                                          
         BE    FILT100                                                          
FILT091  CLI   0(R1),0                                                          
         BE    FILT100                                                          
         CLI   0(R1),X'FF'                                                      
         BE    FILT100                                                          
         CLC   1(4,R1),ADRSTTM                                                  
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         B     FILT091                                                          
*                                                                               
FILT100  LA    R1,FETIME                                                        
         CLI   0(R1),0                                                          
         BE    FILT110                                                          
FILT101  CLI   0(R1),0                                                          
         BE    FILT110                                                          
         CLI   0(R1),X'FF'                                                      
         BE    FILT110                                                          
         CLC   1(4,R1),ADRNDTM                                                  
         IC    RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         B     FILT101                                                          
*                                                                               
FILT110  CLI   EXTMC,0                                                          
         BE    FILT120                                                          
*                                                                               
         L     RF,ADRNDTM          END TIME                                     
         L     RE,ADRSTTM          START TIME                                   
         SR    RF,RE                                                            
         ST    RF,FULL             HALF=END-START                               
*                                                                               
         CLC   FULL,EXTM           EXECUTE TIME                                 
         IC    R1,EXTMC                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         BC    0,FILT120                                                        
         B     EXITNEQ                                                          
*                                                                               
FILT120  B     EXITEQU                                                          
         EJECT                                                                  
*****************************************************************               
*   0(8,R6)=LINEADDR  8(R6)=OV  9(R6)=SE  10(R6)=PRG 11(R6)=TSK *               
*   12(4,R6)=SIN 24(4,R6)=CPU  34(2,R6)=IOS                     *               
*****************************************************************               
*        LA    R1,4(R1)                                                         
*        CLC   0(4,R1),=C'XXXX'                                                 
*        BNE   FLT1                                                             
*                                                                               
*        CLC   0(8,R6),=C'TPHA200S'                                             
*        BE    SYS1                                                             
*        CLC   0(8,R6),=C'UXFR000S'                                             
*        BE    SYS1                                                             
*        BNE   NEWREC                                                           
*        CLI   8(R6),X'0A'         CONT                                         
*        BNE   NEWREC                                                           
*        CLI   9(R6),X'01'         SERV                                         
*        BNE   NEWREC                                                           
*        B     SYS1                                                             
*        CLI   10(R6),X'05'        TOP                                          
*        BNE   NEWREC                                                           
*        CLI   10(R6),X'57'        JOB                                          
*        BE    SYS1                                                             
*        BNE   NEWREC                                                           
*        CLC   0(8,R6),=C'LILO303T'                                             
*        BE    SYS1                                                             
*        CLC   0(4,R6),=C'BOLO'                                                 
*        BE    SYS1                                                             
*        B     NEWREC                                                           
*        CLC   20(4,R6),=X'004E9B70'                                            
*        BL    NEWREC                                                           
*        B     SYS1                                                             
*        CLC   28(2,R6),=X'0010'   QUEUE                                        
*        BL    NEWREC                                                           
*        B     SYS1                                                             
*        CLC   24(4,R6),=F'10'     0.1 SEC CPU                                  
*        BL    NEWREC                                                           
*        TM    30(R6),X'80'                                                     
*        BNO   NEWREC                                                           
*                                                                               
*        XC    FULL,FULL                                                        
*        MVC   FULL+2(2),34(R6)    FULL = IOS                                   
*        OC    FULL,FULL                                                        
*        BNZ   *+10                                                             
*        MVC   FULL,=F'1'          SET IO TO 1 IF ZERO                          
*        SR    RE,RE                                                            
*        ICM   RF,15,24(R6)        RE,RF=CPU                                    
*        D     RE,FULL                                                          
*        C     RF,=F'5'            IF > 0.05 SEC PER IO PRINT                   
*        BL    NEWREC                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD A PRINT LINE FROM ADRREC @ R6                *                   
*************************************************************                   
         SPACE 1                                                                
PRINT    NTR1                                                                   
         USING ADRREC,R6                                                        
         LA    R7,PLINE            R7=A(PLINE)                                  
         USING PRLINE,R7                                                        
         MVC   TRMID,ADRSYM        LINE ADDR                                    
*                                                                               
         SR    R1,R1               LOCATE OV SYS NAME                           
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   0(1,R1),ADROVSYS                                                 
         BE    PRN010                                                           
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
PRN010   MVC   OVSYS,9(R1)         OV SYSTEM                                    
*                                                                               
         SR    R1,R1               LOCATE SE SYS NAME                           
         L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   7(1,R1),ADRSYSNO                                                 
         BE    PRN020                                                           
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
PRN020   MVC   SYSTEM,0(R1)        SE SYSTEM                                    
         MVC   FULL,24(R1)         A(PROG TABLE)                                
*                                                                               
         L     R1,FULL             LOCATE PROGRAM NAME                          
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   8(1,R1),ADRPRGNO                                                 
         BE    PRN030                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FULL(1),ADRPRGNO                                                 
         GOTO1 =V(HEXOUT),DMCB,FULL,PROGRAM,1                                   
         B     *+10                                                             
PRN030   MVC   PROGRAM,0(R1)       PROGRAM                                      
*                                                                               
         MVC   TASK,=C'#.'                                                      
         MVC   TASK+1(1),ADRTASK   TASK ID                                      
*                                                                               
         MVC   FULL,ADRINTM                                                     
         BAS   RE,TIMEOUT                                                       
         MVC   ITIME,WORK1         IN TIME                                      
*                                                                               
         MVC   FULL,ADRSTTM                                                     
         BAS   RE,TIMEOUT                                                       
         MVC   STIME,WORK1         START TIME                                   
*                                                                               
         MVC   FULL,ADRNDTM                                                     
         BAS   RE,TIMEOUT                                                       
         MVC   ETIME,WORK1         END TIME                                     
*                                                                               
         L     R1,ADRNDTM                                                       
         SL    R1,ADRINTM                                                       
         ST    R1,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   ELAPSED,WORK1       TOTAL TIME                                   
*                                                                               
         MVC   FULL,ADRCPUTM                                                    
         BAS   RE,TIMEOUT                                                       
         MVC   CPU,WORK1           CPU TIME                                     
*                                                                               
         MVC   FULL,ADRSIN         SIN                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,SIN,4                                       
         MVC   HALF,ADRMSGI                                                     
         GOTO1 =V(HEXOUT),DMCB,HALF,INPUT,2                                     
         MVC   HALF,ADRMSGO                                                     
         GOTO1 =V(HEXOUT),DMCB,HALF,OUTPUT,2                                    
         GOTO1 =V(HEXOUT),DMCB,ADRIOCNT,IOS,L'ADRIOCNT                          
         MVC   HALF,ADROVCNT                                                    
         GOTO1 =V(HEXOUT),DMCB,HALF,OVS,1                                       
*??      MVC   DATE,52(R6)                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***************************************************                             
*        CLOSE FILES AND EXIT                     *                             
***************************************************                             
         SPACE 1                                                                
DISKEND  EQU   *                                                                
*                                                                               
TSTEND   CLOSE (SYSPRINT)                                                       
         CLOSE (ADRIN)                                                          
         XBASE                                                                  
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
         DC    C'INPUT  ',AL1(4,0),X'0000',AL3(IPTYP)                           
         DC    C'LUID   ',AL1(3,8),X'4400',AL3(FLUIDC)                          
         DC    C'SYSTEM ',AL1(5,0),X'C000',AL3(VALOVS)                          
         DC    C'SESYS  ',AL1(4,0),X'C000',AL3(VALSYS)                          
         DC    C'PROGRAM',AL1(6,0),X'C000',AL3(VALPROG)                         
         DC    C'TASK   ',AL1(3,0),X'6000',AL3(FTASKC)                          
         DC    C'SIN    ',AL1(2,4),X'7400',AL3(FSINC)                           
         DC    C'STIME  ',AL1(4,4),X'6600',AL3(FSTIME)                          
         DC    C'ETIME  ',AL1(4,4),X'6600',AL3(FETIME)                          
         DC    C'CPUTIME',AL1(6,0),X'6800',AL3(FCPUC)                           
         DC    C'QTIME  ',AL1(4,0),X'6800',AL3(FQTMC)                           
         DC    C'XTIME  ',AL1(4,0),X'6800',AL3(EXTMC)                           
         DC    C'OICOUNT',AL1(6,0),X'6800',AL3(FIOCC)                           
         DC    C'OVCOUNT',AL1(6,0),X'6800',AL3(FOVCC)                           
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
IPTYP    DC    C'TAPE'             INPUT=TAPE                                   
*                                                                               
FTASKC   DC    X'00'                                                            
FTASK    DC    C' '                TASK FILTER                                  
FIOCC    DC    X'00'                                                            
FIOC     DC    XL2'0000'           I/O COUNT FILTER                             
FOVCC    DC    X'00'                                                            
FOVC     DC    XL2'0000'           OV COUNT FILTER                              
FQTMC    DC    X'00'                                                            
FQTM     DC    XL2'0000'           QUEUE TIME                                   
EXTMC    DC    X'00'                                                            
EXTM     DC    XL4'0000'           EXECUTE TIME                                 
FCPUC    DC    X'00'                                                            
FCPU     DC    XL2'0000'           CPU TIME                                     
*                                                                               
FSESYS   DC    X'00'                                                            
FOVSYS   DC    X'00'                                                            
FPROG    DC    X'00'                                                            
*                                                                               
FLUIDC   DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'FF'                                                            
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
         LA    RF,PLINE+1                                                       
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
         L     RF,TUHOUR                                                        
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,TUMINUTE                                                      
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,TUSECOND                                                      
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         L     RF,TUMSEC                                                        
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
***************************************************                             
*        ERROR HANDLING                           *                             
***************************************************                             
         SPACE 1                                                                
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,TUHOUR                                                        
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMINUTE                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUSECOND                                                      
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
TITLE    DC    CL166' '                                                         
         ORG   TITLE                                                            
         DC    C'1',X'40'                                                       
         DC    CL12'Arrival Time'                                               
         DC    CL01' '                                                          
         DC    CL11'Start Time'                                                 
         DC    CL02' '                                                          
         DC    CL11'End Time'                                                   
         DC    CL02' '                                                          
         DC    CL08'Terminal'                                                   
         DC    CL02' '                                                          
         DC    CL03'Sys'                                                        
         DC    CL02' '                                                          
         DC    CL07'SE Sys'                                                     
         DC    CL02' '                                                          
         DC    CL07'Program'                                                    
         DC    CL02' '                                                          
         DC    CL02'Tk'                                                         
         DC    CL02' '                                                          
         DC    CL08'SIN'                                                        
         DC    CL02' '                                                          
         DC    CL11'CPU Time'                                                   
         DC    CL02' '                                                          
         DC    CL12'Elapsed Time'                                               
         DC    CL01' '                                                          
         DC    CL04'I/P'                                                        
         DC    CL02' '                                                          
         DC    CL04'O/P'                                                        
         DC    CL02' '                                                          
         DC    CL06'#IOs'                                                       
         DC    CL02' '                                                          
         DC    CL04'#OVs'                                                       
         DC    CL02' '                                                          
*??      DC    CL07'Date'                                                       
         ORG                                                                    
TITLE1   DC    CL166' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'-----------------------------------------------------'         
         DC    C'------------------ PARAMETER CARDS ------------------'         
         DC    C'-----------------------------------------------------'         
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        LRECL=(166) USE CHARS=(BX15)                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
ADRIN    DCB   DSORG=PS,MACRF=GL,DDNAME=ADRIN,EODAD=ADREND                      
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
TUHOUR   DC    F'138240000'        60*60*38400                                  
TUMINUTE DC    F'2304000'          60*38400                                     
TUSECOND DC    F'38400'            38400                                        
TUMSEC   DC    F'384'              384                                          
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL166' '                                                         
       ++INCLUDE FASYSLST                                                       
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        TABLES                                             *                   
*************************************************************                   
         SPACE 1                                                                
LUIDTAB  DC    CL8'WGLO156T'                                                    
         DC    CL8'LHLO216T'                                                    
**       DC    CL8'MOMA206T'                                                    
         DC    CL8'TDLO406T'                                                    
**       DC    CL8'TDLO426T'                                                    
         DC    CL8'GHLO206T'                                                    
         DC    CL8'JWLO326T'                                                    
         DC    CL8'YOLOH06T'                                                    
         DC    CL8'ADLE0C6T'                                                    
         DC    CL8'TDLO415T'                                                    
         DC    CL8'CTLO005T'                                                    
         DC    CL8'TDLO405T'                                                    
**       DC    CL8'BHLO405T'                                                    
         DC    CL8'JWLO305T'                                                    
         DC    CL8'WCLO305T'                                                    
         DC    CL8'GHLO205T'                                                    
         DC    CL8'JOLO115T'                                                    
         DC    CL8'TDLO425T'                                                    
         DC    CL8'LHLO215T'                                                    
**       DC    CL8'DOLO6C5T'                                                    
         DC    CL8'JOLO105T'                                                    
         DC    CL8'JWLO325T'                                                    
         DC    CL8'BOLO115T'                                                    
**       DC    CL8'MOMA205T'                                                    
         DC    CL8'TDLO414T'                                                    
         DC    CL8'MOMA104T'                                                    
         DC    CL8'BHLO404T'                                                    
         DC    CL8'LHLO234T'                                                    
         DC    CL8'WCLO304T'                                                    
**       DC    CL8'TDMA0C4T'                                                    
         DC    XL8'00'                                                          
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
CARDRD   DS    F                                                                
CARDR2   DS    F                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
CARDEND  DS    A                                                                
*                                                                               
GTAPGMS  DS    A                                                                
GTPROG   DS    X                                                                
GTPROGN  DS    CL8                                                              
GTSYS    DS    X                                                                
GTSYSN   DS    CL8                                                              
GTSYSSE  DS    X                                                                
*                                                                               
PLINE    DS    CL166                                                            
*                                                                               
ADRBUFF  DS    4096C               4K BUFFER                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
PRLINE   DSECT                                                                  
         DS    0CL166                                                           
PRASA    DS    CL1                                                              
         DS    CL1                                                              
ITIME    DS    CL11                                                             
         DS    CL2                                                              
STIME    DS    CL11                                                             
         DS    CL2                                                              
ETIME    DS    CL11                                                             
         DS    CL2                                                              
TRMID    DS    CL8                                                              
         DS    CL2                                                              
OVSYS    DS    CL3                                                              
         DS    CL2                                                              
SYSTEM   DS    CL7                                                              
         DS    CL2                                                              
PROGRAM  DS    CL7                                                              
         DS    CL2                                                              
TASK     DS    CL2                                                              
         DS    CL2                                                              
SIN      DS    CL8                                                              
         DS    CL2                                                              
CPU      DS    CL11                                                             
         DS    CL2                                                              
ELAPSED  DS    CL11                                                             
         DS    CL2                                                              
INPUT    DS    CL4                                                              
         DS    CL2                                                              
OUTPUT   DS    CL4                                                              
         DS    CL2                                                              
IOS      DS    CL6                                                              
         DS    CL2                                                              
OVS      DS    CL4                                                              
         DS    CL2                                                              
DATE     DS    CL7                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FAADRREC                                                       
*FADSYSLSTD                                                                     
       ++INCLUDE FASYSLSTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'163DDTRMORA  05/01/02'                                      
         END                                                                    
