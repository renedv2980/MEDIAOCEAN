*          DATA SET DDHEXCON   AT LEVEL 002 AS OF 02/13/03                      
*PHASE HEXCONA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE SCANNER                                                                
*****************************************************************               
* DDHEXCON  - MODULE TO CONVERT A HEXADECIMAL STRING IN A       *               
* DATA FILE TO A BINARY STRING FOR LOADING INTO DB2 FOR OS/390  *               
*                                                               *               
* DATASETS - INFILE - INPUT DATA TO DDHEXCON                    *               
*            OUTFILE - OUTPUT DATA FROM DDHEXCON                *               
*                                                               *               
* CONTROL CARD                                                  *               
* RECORD ID,COLUMN POSITION, HEX STRING LENGTH                  *               
* E.G. 05000,15,38 FOR 05000 RECORDS, COLUMN POSITION 15, AND   *               
*      HEX STRING LENGTH OF 38                                  *               
*****************************************************************               
         TITLE 'CONVERT HEXADECIMAL STRING TO BINARY STRING'                    
HEXCON   CSECT                                                                  
         PRINT GEN                                                              
         NBASE WRKX-WRKD,**HEXCON,REGSAVE,R9,R8,CLEAR=YES                       
         USING WRKD,RC                                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(40),=CL40'CONVERT HEXADECIMAL STRING'                      
         BAS   RE,GENINIT                                                       
*                                                                               
         MVC   P(80),=CL80'BEFORE VALCARDS'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,VALCARDS                                                      
         BNE   EXIT                                                             
*                                                                               
         MVC   P(80),=CL80'BEFORE PROCFILE'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,PROCFILE                                                      
         BNE   EXIT                                                             
*                                                                               
* PRINT OUT SUMMARY COUNTS                                                      
*                                                                               
         MVC   P(20),=CL20'RECORDS READ'                                        
         EDIT  INPUT,(9,P+20)                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(20),=CL20'NO CHANGES OUTPUT'                                   
         EDIT  NOCHANGE,(9,P+20)                                                
         GOTO1 VPRINTER                                                         
         MVC   P(20),=CL20'CHANGES OUTPUT'                                      
         EDIT  CHANGES,(9,P+20)                                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   RETCDE,ZERO         RETURN RC=0 TO MVS                           
*                                                                               
EXIT     XBASE RC=RETCDE                                                        
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PROCESS THE INPUT FILE AND PERFORM HEXADECIMAL      *          
* STRING CONVERSIONS WHERE NECESSARY                                 *          
* AT EXIT, CC=EQ IF DSN OPEN, CC=NEQ IF DATA SET NOT OPEN            *          
**********************************************************************          
         SPACE 2                                                                
PROCFILE NTR1   ,                                                               
         L     R2,AINFILE          OPEN INPUT AND OUTPUT FILES                  
         OPEN  ((R2),INPUT)                                                     
         L     R3,AOUTFILE                                                      
         OPEN  ((R3),(OUTPUT))                                                  
         SPACE 1                                                                
PRFILE10 L     R4,AINLEN                                                        
         GET   (R2),(R4)           GET INPUT RECORD                             
         L     R5,INPUT                                                         
         AHI   R5,1                INCREMENT INPUT COUNTER                      
         ST    R5,INPUT                                                         
         AHI   R4,L'INLENGTH       R4=A(INPUT RECORD)                           
         CLC   RECID,0(R4)         APPLY RECORD NUMBER FILTER                   
         BE    PRFILE20            YES-IT MATCHES                               
*                                                                               
         L     R4,AINLEN                                                        
         PUT   (R3),(R4)           NO-PUT AS IS TO OUTPUT FILE                  
         L     R5,NOCHANGE                                                      
         AHI   R5,1                INCREMENT NOCHANGE COUNTER                   
         ST    R5,NOCHANGE                                                      
         B     PRFILE10                                                         
*                                                                               
PRFILE20 BAS   RE,CONREC                                                        
         BNE   NO                                                               
         L     R4,AOUTLEN                                                       
         PUT   (R3),(R4)                                                        
         L     R5,CHANGES                                                       
         AHI   R5,1                INCREMENT CHANGED RECORDS COUNTER            
         ST    R5,CHANGES                                                       
         B     PRFILE10                                                         
*                                                                               
PRFILE99 CLOSE ((R2))                                                           
         CLOSE ((R3))                                                           
         B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PROCESS AN INPUT RECORD AND CONVERT THE HEXADECIMAL *          
* STRING                                                             *          
* AT ENTRY, AINLEN AND AINREC POINT TO INPUT RECORD                  *          
*           FIELDNUM IS INDEX TO HEX STRING, HEXLEN IS LEN OF STRING *          
* ON EXIT, AOUTLEN AND AOUTPUT WILL POINT TO OUTPUT RECORD LENGTH    *          
*          AND THE OUTPUT RECORD WITH THE CONVERTED STRING           *          
**********************************************************************          
         SPACE 2                                                                
CONREC   NTR1   ,                                                               
         L      R4,AOUTREC         R4=A(OUTPUT RECORD)                          
         L      R2,AOUTLEN         R2=A(OUTPUT LENGTH)                          
         XC     0(4,R2),0(R2)      ZERO OUTPUT LENGTH                           
         L      R3,AINLEN          R3=A(INPUT LENGTH)                           
         LH     RF,0(R3)           RF=INPUT LENGTH                              
         SHI    RF,4                                                            
         STH    RF,INPUTLEN        SAVE INPUT RECORD LEN W/O HEADER             
         L      R5,AINREC          R5=A(INPUT RECORD)                           
*                                                                               
* FIND THE HEX STRING IN THE INPUT RECORD                                       
*                                                                               
CONREC10 L      RE,FIELDNUM        GET FIELD NUMBER                             
         LR     R6,R5              R6=A(HEX STRING)                             
         SHI    RE,1               ITS ONE BASED SO DECREMENT IT                
         BZ     CONREC20           ALREADY POINTING AT HEX STRING               
*                                                                               
CONREC12 CLI    0(R6),DELIM        TEST FOR FIELD DELIMITER                     
         BE     CONREC15                                                        
CONREC13 LA     R6,1(R6)                                                        
         B      CONREC12                                                        
*                                                                               
CONREC15 BCT    RE,CONREC13                                                     
         LA     R6,1(R6)                                                        
*                                                                               
* R6 NOW POINTS TO START OF HEX STRING-COPY RECORD BEFORE STRING                
*                                                                               
CONREC20 LR    RE,R4               RE=A(OUTPUT RECORD)                          
         LR    RF,R6               RF=A(HEX STRING)                             
         SR    RF,R5               COMPUTE DISP TO HEX STRING                   
         STH   RF,HALF             SAVE DISP TO HEX STRING                      
         L     R0,AINREC           R0=SOURCE OF MOVE                            
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE DATA BEFORE HEX STRING                  
*                                                                               
* CONVERT THE HEX STRING AND SLOT IT INTO OUTPUT AREA                           
*                                                                               
CONREC30 AH    R4,HALF             POINT R4(O/P POINTER) TO DEST                
         L     R7,HEXLEN           R7=HEX STRING LENGTH                         
         GOTO1 VHEXIN,DMCB,(R6),(R4),(R7)                                       
         ICM   RE,15,DMCB+12       GET LENGTH OF OUTPUT STRING                  
         BZ    CONRECR             WE HAVE A SERIOUS ERROR                      
*                                                                               
         AR    R4,RE               ADVANCE R4 PAST HEXIN OUTPUT                 
         A     R6,HEXLEN           POINT I/P POINTER PAST HEX STRING            
*                                                                               
* COPY REST OF INPUT RECORD AFTER HEX STRING                                    
*                                                                               
CONREC40 LR    RE,R6               RE=A(NEXT BYTE TO MOVE)                      
         S     RE,AINREC           RE=DISP TO NEXT BYTE TO MOVE                 
         LH    RF,INPUTLEN         COMPUTE NUMBER OF BYTES LEFT TO              
         SR    RF,RE               MOVE IN RF                                   
         BZ    CONREC50            NOTHING LEFT TO MOVE                         
*                                                                               
         LR    R0,R4               R0=O/P POINTER                               
         LR    RE,R6               RE=I/P POINTER                               
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY REST OF DATA                            
*                                                                               
* SET THE OUTPUT LENGTH                                                         
*                                                                               
CONREC50 LH    RE,INPUTLEN         RE=LENGTH OF INPUT RECORD                    
         L     RF,HEXLEN                                                        
         SRL   RF,1                RF=LENGTH OF BINARY STRING                   
         SR    RE,RF               ADJUST INPUT RECORD LENGTH                   
         AHI   RE,4                ADD BACK HEADER LENGTH                       
         L     R1,AOUTLEN                                                       
         STH   RE,0(R1)                                                         
*                                                                               
CONRECX  B     YES                                                              
*                                                                               
* ERROR EXIT FOR HEXIN CONVERSION ERROR                                         
*                                                                               
CONRECR  MVI   ERROR,2                                                          
         BAS   RE,ERRPRT                                                        
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1  ,                                                                
         MVC   RETCDE,EIGHT        PRESET RETURN CODE TO 0                      
*                                                                               
         MVC   RECID,SPACES        INITIALIZE THREE PARAM CARD                  
         XC    FIELDNUM,FIELDNUM   VALUES                                       
         XC    HEXLEN,HEXLEN                                                    
*                                                                               
         L     RE,AINREC           CLEAR INPUT AND OUTPUT AREAS                 
         LA    RF,L'INRECORD                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AOUTREC                                                       
         LA    RF,L'OUTREC                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1  ,                                                                
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,C,=C'RE00'                                           
         CLC   =C'/*',C            IF END OF JCL                                
         BNE   VCLP5                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         CLC   RECID,SPACES        TEST IF RECID VALUE PRESENT                  
         BE    VCERR1              NO-FLAG AN ERROR                             
         B     VCEND                                                            
*                                                                               
VCLP5    GOTO1 VSCANNER,DMCB,(C'C',C),WORK,0                                    
         CLI   DMCB+4,0            TEST FOR NO INPUT                            
         BE    VCERR1                                                           
         CLI   DMCB+4,3            TEST FOR 3 FIELDS IN CARD                    
         BNE   VCERR2              NO-MUST BE INVALID                           
*                                                                               
         LA    R4,WORK             R4=A(SCANNER OUTPUT BLOCK)                   
         USING SCANBLKD,R4                                                      
*                                                                               
VCLP10   CLI   SC2NDLEN,0          TEST FOR SINGLE FIELD                        
         BH    VCERR2              NO                                           
         CLI   SC1STLEN,L'RECID    TEST FIELD CAN ONLY BE RECID                 
         BH    VCERR2                                                           
*                                                                               
         MVC   RECID,SCONEFLD      EXTRACT RECORD ID VALUE                      
*                                                                               
VCLP20   LA    R4,SCBLKLQ(R4)      BUMP TO NEXT SCANNER BLOCK ENTRY             
         CLI   SC2NDLEN,0          TEST FOR SINGLE FIELD                        
         BH    VCERR2                                                           
         CLI   SC1STLEN,0          TEST FOR SOME DATA                           
         BE    VCERR2              NO-ITS INVALID                               
         CLI   SC1STLEN,2          TEST MORE THAN 2 DIGITS                      
         BH    VCERR2              YES-CAN'T BE GOOD                            
         TM    SC1STVAL,SCNUMQ     TEST FOR VALID NUMBER                        
         BZ    VCERR2                                                           
         MVC   FIELDNUM,SC1STNUM   EXTRACT COLUMN NUMBER                        
*                                                                               
VCLP30   LA    R4,SCBLKLQ(R4)      ADVANCE TO NEXT SCANNER BLOCK                
         CLI   SC2NDLEN,0          TEST FOR SINGLE FIELD                        
         BH    VCERR2                                                           
         CLI   SC1STLEN,0          TEST FOR SOME DATA                           
         BE    VCERR2              NO-ITS INVALID                               
         CLI   SC1STLEN,2          TEST MORE THAN 2 DIGITS                      
         BH    VCERR2              YES-CAN'T BE GOOD                            
         TM    SC1STVAL,SCNUMQ     TEST FOR VALID NUMBER                        
         BZ    VCERR2                                                           
         MVC   HEXLEN,SC1STNUM     EXTRACT HEX STRING LENGTH                    
*                                                                               
VCEND    B     VCYES                                                            
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   MVI   ERROR,0                                                          
         B     VCERR                                                            
VCERR2   MVI   ERROR,1                                                          
         B     VCERR                                                            
*                                                                               
VCERR    BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCYES    B     YES                 EXIT OK                                      
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
* AT ENTRY, ERROR = ERROR MESSAGE NUMBER TO PRINT                     *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1  ,                                                                
         LA    RE,ERRTAB                                                        
         ZIC   RF,ERROR                                                         
         MHI   RF,L'ERRTAB                                                      
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRTAB),0(RE)                                             
         GOTO1 VPRINTER                                                         
         MVI   ERROR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
* PROGRAM CONSTANTS DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
DELIM    EQU   C';'                                                             
*                                                                               
         DS    0H                                                               
ERRTAB   DS    0CL40               ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'MISSING CONTROL CARD'                                       
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'INVALID HEXADECIMAL STRING'                                 
         SPACE 1                                                                
*                                                                               
AINFILE  DC    A(INFILE)                                                        
AOUTFILE DC    A(OUTFILE)                                                       
*                                                                               
AINLEN   DC    A(INLENGTH)                                                      
AINREC   DC    A(INRECORD)                                                      
AOUTLEN  DC    A(OUTLEN)                                                        
AOUTREC  DC    A(OUTREC)                                                        
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VPRINTER DC    V(PRINTER)                                                       
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
RETCDE   DS    F                                                                
*                                                                               
* INPUT AND OUTPUT FILES MATCH EXTRACT FILE CHARACTERISTICS                     
*                                                                               
INFILE   DCB   DDNAME=INFILE,DSORG=PS,MACRF=(GM),RECFM=VB,             X        
               EODAD=PRFILE99                                                   
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=VB,MACRF=(PM)                      
*                                                                               
C        DC    CL80' '                                                          
*                                                                               
ZERO     DC    F'0'                0                                            
FOUR     DC    F'4'                4                                            
EIGHT    DC    F'8'                8                                            
*                                                                               
INPUT    DC    F'0'                                                             
NOCHANGE DC    F'0'                                                             
CHANGES  DC    F'0'                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*INPUT**'                                                    
INLENGTH DC    F'0'                                                             
INRECORD DS    CL2048                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*OUTPUT*'                                                    
OUTLEN   DC    F'0'                                                             
OUTREC   DS    CL2048                                                           
*                                                                               
REGSAVE  DS    20000D                                                           
*                                                                               
**********************************************************************          
* PROGRAM VARIABLES DECLARATIONS                                     *          
**********************************************************************          
*                                                                               
WRKD     DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6A                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
ERROR    DS    XL1                                                              
         DS    0D                                                               
WORK     DS    CL256                                                            
*                                                                               
RECID    DS    CL5                                                              
FIELDNUM DS    F                                                                
HEXLEN   DS    F                                                                
*                                                                               
RETCODE  DS    F                   CHEKCODE SAVES RF HERE                       
INPUTLEN DS    H                                                                
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINTL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDHEXCON  02/13/03'                                      
         END                                                                    
