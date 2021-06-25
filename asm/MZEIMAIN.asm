*          DATA SET MZEIMAIN   AT LEVEL 254 AS OF 09/29/98                      
*PHASE MZEIMAIN                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PANIC                                                                  
*INCLUDE CARDS                                                                  
*                                                                               
         TITLE 'DISASSEMBLER PROTOTYPE MARK I'                                  
SAMPLE   CSECT                                                                  
*                                                                               
         PRINT GEN                                                              
         NBASE 0,*SAMPLE*,=V(REGSAVE),R8                                        
         PRINT NOGEN                                                            
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
*                                    COVER PAGE                                 
*                                                                               
        MVC   P(22),=C'THIS IS THE COVER PAGE'                                  
        GOTO1 =V(PRINTER)                                                       
        MVC   LINE,=PL2'60'                                                     
***********************************************************                     
*//////////////////////////////////////////////////////////                     
*               PAN READ MODULE                                                 
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\                     
***********************************************************                     
*                                                                               
*          DATA SET SYEAPAN    AT LEVEL 074 AS OF 09/17/98                      
***********************************************************                     
*                                                                               
         LA    R4,0             INDIC FOR PRINTOUT (TEMPORARY)                  
INSTLOOP DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    LOADRTN                                                          
         CLC   CARD(L'PANTEST),PANTEST                                          
         BNE   TEST002                                                          
         MVC   PANNAME,CARD+L'PANTEST                                           
         B     INSTLOOP                                                         
TEST002  B     ENDRTN                                                           
*                                                                               
LOADRTN  DS    0H                                                               
         LA    R2,0              INDICATOR FOR ESD STATEMENTS                   
         ST    R2,BUFLEN         INITIALIZE LENGTH OF BUFFER                    
         STH   R2,LOCATION       INITIALIZE LOCATION COUNTER                    
         LA    R5,BUFFER         POINTER TO BUFFER                              
LOADLOOP DS    0H                                                               
         GOTO1 =V(PANIC),DMCB,(X'80',=C'READ'),=C'PAN',PANNAME,CARD             
         LA    R4,1(R4)                                                         
         CLC   CARD(2),=C'/*'     IS THIS END OF FILE                           
         BE    DECODE           ***** PRTBUF                                    
         CLC   CARD+1(3),=C'ESD'                                                
         BNE   IFTEXT                                                           
         CH    R2,=H'0'           IS THIS 1ST ESD STMT                          
         BNE   IFTEXT             IF NOT CHECK FOR TEXT                         
         MVC   CNAME,CARD+16      IF IT IS, GET CSECT NAME                      
         LA    R2,1(R2)           INDICATE CSECT NAME GOTTEN                    
         B     LOADLOOP           GET NEW RECORD                                
*                                                                               
IFTEXT   DS    0H                                                               
         CLC   CARD+1(3),=C'TXT'  IS THIS A TEXT LINE                           
         BNE   LOADLOOP           IF NOT, GET NEW RECORD                        
         MVC   HALF,CARD+10       ELSE, GET LEN OF RECORD                       
         LH    R6,HALF            & LOAD INTO REGISTER                          
         BCTR  R6,0               DECREMENT                                     
         EX    R6,MYPUT           & EXECUTE MOVE TO BUFFER                      
MYPUT    MVC   0(0,R5),CARD+16                                                  
         LA    R6,1(R6)           INCREMENT R6 TO CORRECT DECR                  
         AR    R5,R6              MOVE R5 ALONG SO MANY BYTES                   
         L     R0,BUFLEN          GET THE OLD BUFFER LENGTH                     
         AR    R0,R6              ADD LENGTH OF THIS LINE                       
         ST    R0,BUFLEN          STORE NEW BUFFER LENGTH                       
         B     LOADLOOP           THEN GET ANOTHER RECORD                       
*                                                                               
PRTBUF   DS    0H                                                               
         LA    R5,BUFFER                                                        
ENDLOOP  DS    0H                                                               
         MVC   CARD(38),0(R5)                                                   
*        GOTO1 =V(HEXOUT),CARD,P,38                                             
         MVC   P(38),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         LA    R5,38(R5)                                                        
         BCT   R4,ENDLOOP                                                       
***********************************************************                     
*//////////////////////////////////////////////////////////                     
*               END PAN READ MODULE                                             
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\                     
***********************************************************                     
*                                                                               
DECODE   DS    0H                                                               
         USING CODESD,R9           USE CODESD DSECT TO READ FROM                
*                                                                               
         LA    R5,BUFFER           POINT R5 TO BUFFER                           
*                                                                               
         MVC   P(L'CNAME),CNAME                                                 
         MVC   MNEOUT(5),=C'CSECT'                                              
         GOTO1 =V(PRINTER)                                                      
***********************************************************************         
*        TRY TO MATCH UP INPUTTED INSTRUCTION TO TABLE INSTRUCTION              
***********************************************************************         
*                                                                               
READLOOP DS    0H                                                               
*                                                                               
         L     RE,BUFLEN                                                        
         CH    RE,=H'0'                                                         
         BE    ENDRTN              STOP IF COUNTER HAS HIT ZERO                 
         BNH   ENDRTN              OR GONE BELOW ZERO                           
         LA    R9,TYPETAB          MOVE DSECT TO BEGINNING OF TABLE             
         ZIC   R0,0(R5)            MOVE IST BYTE OF INPUT TO BUFCODE            
         STH   R0,BUFCODE                                                       
*                                                                               
COMLOOP  DS    0H                                                               
         CLI   CODED,X'FF'         IF AT END OF TYPE TABLE AND NO               
         BE    MYSTRY              OP CODE FOUND GOTO MYSTRY                    
*                                                                               
         CLC   BUFCODE,CODED       COMPARE HIGH ORDER BYTE OF INPUT             
         BE    FOUND               TO CODE OF CURRENT TABLE INSTRUCT.           
*                                  IF EQUAL GOTO FOUND                          
*                                                                               
         LA    R9,CODESLQ(R9)      IF NOT FOUND THEN CHECK THE NEXT             
         B     COMLOOP             TABLE ENTRY                                  
                                                                                
*                                                                               
***********************************************************************         
*        INSTRUCTION HAS BEEN FOUND!                                            
***********************************************************************         
*                                                                               
FOUND    DS    0H                                                               
*                                                                               
         CLI   FORMATD,X'1'      WHAT TYPE OF INSTRUCTION IS THIS               
         BNE   CHK2              IF NOT THIS TYPE, CHECK NEXT                   
         GOTO1 =A(TYPE1RT)       EXECUTE APPROPRIATE SUB-RTN                    
         LA    R5,L'RX(R5)       MOVE POINTER  LENGTH OF INSTR                  
         LA    RE,L'RX           RTN TO DECREMENT BUFFER CNTR                   
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK2     DS    0H                 PROCEDURE SAME AS ABOVE                       
         CLI   FORMATD,X'2'                                                     
         BNE   CHK3                                                             
         GOTO1 =A(TYPE2RT)                                                      
         LA    R5,L'RS1(R5)                                                     
         LA    RE,L'RS1                                                         
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK3     DS    0H                PROCEDURE SAME AS ABOVE                        
         CLI   FORMATD,X'3'                                                     
         BNE   CHK4                                                             
         GOTO1 =A(TYPE3RT)                                                      
         BE    TYPE3RT                                                          
         LA    R5,L'RS2(R5)                                                     
         LA    RE,L'RS2                                                         
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                               PROCEDURE SAME AS ABOVE                         
CHK4     DS    0H                                                               
         CLI   FORMATD,X'4'                                                     
         BNE   CHK5                                                             
         GOTO1 =A(TYPE4RT)                                                      
         LA    R5,L'RR(R5)                                                      
         LA    RE,L'RR                                                          
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                 PROCEDURE SAME AS ABOVE                       
CHK5     DS    0H                                                               
         CLI   FORMATD,X'5'                                                     
         BNE   CHK6                                                             
         GOTO1 =A(TYPE5RT)                                                      
         BE    TYPE5RT                                                          
         LA    R5,L'SI(R5)                                                      
         LA    RE,L'SI                                                          
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK6     DS    0H                 PROCEDURE SAME AS ABOVE                       
         CLI   FORMATD,X'6'                                                     
         BNE   CHK7                                                             
         GOTO1 =A(TYPE6RT)                                                      
         BE    TYPE6RT                                                          
         LA    R5,L'SS1(R5)                                                     
         LA    RE,L'SS1                                                         
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK7     DS    0H                 PROCEDURE SAME AS ABOVE                       
         CLI   FORMATD,X'7'                                                     
         BNE   CHK8                                                             
         GOTO1 =A(TYPE7RT)                                                      
         LA    R5,L'SS2(R5)                                                     
         LA    RE,L'SS2                                                         
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK8     DS    0H                 PROCEDURE SAME AS ABOVE                       
         CLI   FORMATD,X'8'                                                     
         BNE   CHK9                                                             
         GOTO1 =A(TYPE8RT)                                                      
         LA    R5,L'SS3(R5)                                                     
         LA    RE,L'SS2                                                         
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
         B     PRT                                                              
*                                                                               
CHK9     DS    0H                 PROCEDURE SAME AS ABOVE                       
         CLI   FORMATD,X'9'                                                     
         BNE   MYSTRY             SHOULD NEVER HAPPEN                           
         GOTO1 =A(TYPE9RT)                                                      
         LA    R5,L'RX(R5)                                                      
         LA    RE,L'RX                                                          
         L     R0,BUFLEN                                                        
         SR    R0,RE                                                            
         ST    R0,BUFLEN                                                        
*                                                                               
PRT      DS    0H                                                               
         LR    R3,RE                                                            
         GOTO1 =V(HEXOUT),DMCB,LOCATION,LOCOUT,2                                
         AH    R3,LOCATION                                                      
         STH   R3,LOCATION                                                      
         GOTO1 =V(PRINTER)                                                      
         B     READLOOP                                                         
*                                                                               
**********************************************************                      
* THE FOLLOWING PROCEDURE IS NOT SATISFACTORY AND WILL BE                       
* CHANGED FOR THE MARK II VERSION                                               
**********************************************************                      
*                                                                               
MYSTRY   DS    0H                                                               
         CLI   BUFCODE,X'00'        IS THE BYTE NULLS                           
         BNE   NOTNULL              IF NOT,                                     
         LA    R5,1(R5)             IF IT IS, IGNORE IT                         
         L     R0,BUFLEN            & DECREMENT BUFLEN                          
         BCTR  R0,0                                                             
         ST    R0,BUFLEN                                                        
         B     READLOOP                                                         
*                                                                               
NOTNULL  DS    0H                                                               
         MVC   HALF,0(R5)          MOVE TWO BYTES INTO HALF                     
         GOTO1 =V(HEXOUT),DMCB,HALF,FULL,2                                      
         MVC   DCDATA,FULL         4 BYTES GENERATED TO DATA                    
         MVC   TEXT(L'DCTEXT),DCTEXT                                            
         GOTO1 =V(PRINTER)                                                      
         LA    R5,2(R5)                                                         
         L     R0,BUFLEN                                                        
         SH    R0,=H'2'                                                         
         ST    R0,BUFLEN                                                        
*        B     READLOOP                                                         
*                                                                               
ENDRTN   DS    0H                                                               
         MVC   MNEOUT(3),=C'END'                                                
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
         EJECT                                                                  
*****************************************************************               
* /////////////////////////////////////////////////////////////                 
*                   INTERNAL SUBROUTINES                                        
* /////////////////////////////////////////////////////////////                 
*****************************************************************               
*                                                                               
TYPE1RT  DS    0H                  INSTRUCTION IS AN RX INSTRUCTION             
         NTR1                                                                   
         MVC   RX,0(R5)                                                         
         MVC   MNEOUT,MNEMOND       MOVE MNEMONIC INTO PRINT LINE               
         LA    R2,TEXT                                                          
*                                                                               
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         ZIC   RE,REGIN            MOVE SECOND BYTE OF INPUT INTO RE            
         SRL   RE,4                                                             
         STC   RE,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R2),LOWER         OPERAND 1                                  
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BDISPRX                                                     
         SLL   RE,20                                                            
         SRL   RE,20                                                            
         EDIT  (RE),(4,(R2)),,ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R2,R0               MOVE PTR LENGTH OF OUTPUT                    
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         MVI   BYTE,X'00'                                                       
         MVN   BYTE,REGIN          MOVE LOW-ORDER NIBBLE OF INPUT'S             
*                                  SECOND BYTE (INDEX) INTO BYTE                
         SR    R0,R0               CLEAR REGISTER R0 AND                        
         MVI   0(R2),C'R'          PUT AN 'R' INTO THE INDEX                    
         ICM   R0,1,BYTE           MOVE THE INDEX INTO REGISTER 0               
         BNZ   NOTZERO                                                          
*                                                                               
         MVI   0(R2),C'0'          IF THE INDEX IS ZERO, ZERO OUT               
*                                                                               
NOTZERO  DS    0H                  PUT INDEX INTO THE PRINT LINE                
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         STC   R0,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R2),LOWER         MOVE IN INDEX                              
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BDISPRX        MOVE SECOND BYTE OF INPUT INTO RE            
         SRL   RE,12                                                            
         STC   RE,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R2),LOWER          PUT BASE REG. INTO THE PRINT LINE         
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
*****************************************************                           
*          DATA SET YYUNDIS    AT LEVEL 063 AS OF 09/21/98                      
*****************************************************                           
TYPE2RT  NTR1                      RS INSTRUCTION W/O MASK                      
         MVC   RS1,0(R5)                                                        
         ZIC   R6,REGS2                                                         
         SRL   R6,4                  R6 CONTAINS THE 1ST REGISTER#              
         IC    R7,REGS2                                                         
         SLL   R7,28                                                            
         SRL   R7,28                 R7 CONTAINS THE 2ND REGISTER#              
         ICM   R3,3,BDISRS1                                                     
         SLL   R3,20                                                            
         SRL   R3,20                 R3 CONTAINS THE DISPLACEMENT               
         ZIC   R4,BDISRS1                                                       
         SRL   R4,4                  R4 CONTAINS THE BASE REGISTER              
         MVC   P(L'MNEMOND),MNEMOND  MOVE MNEMONIC INTO PRINT LINE              
         LA    R2,P+L'MNEMOND                                                   
         EDIT  (R6),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R7),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C'('                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R4),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
****************** NEEDED A CONDITIONAL STATEMENT                               
         PRINT  GEN                                                             
         MVI   P,C' '                                                           
         MVC   P+1,P                                                            
         MVC   MNEOUT(L'MNEMOND),MNEMOND  ****REPEATED                          
         LA    R2,TEXT                    ****REPEATED                          
         MVI   0(R2),C'R'                                                       
         LA    R2,1(,R2)                                                        
         GOTO1 =V(HEXOUT),DMCB,REGS2,(R2),1                                     
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         GOTO1 =V(HEXOUT),DMCB,REGS2,(R2),1                                     
         MVI   0(R2),C'R'                                                       
         LA    R2,1(,R2)                                                        
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C'('                                                       
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C'R'                                                       
         LA    R2,1(,R2)                                                        
         GOTO1 =V(HEXOUT),DMCB,BDISRS1,(R2),1                                   
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         PRINT  NOGEN                                                           
*                                                                               
TYPE3RT  DS    0H                                                               
         NTR1                                                                   
         MVC   RS2,0(R5)                                                        
         MVC   P(L'RS2),RS2                                                     
         ZIC   R6,REGMASK                                                       
         SRL   R6,4                                                             
         IC    R7,REGMASK                                                       
         SLL   R7,28                                                            
         SRL   R7,28                                                            
         ICM   R3,3,BDISRS2                                                     
         SLL   R3,20                                                            
         SRL   R3,20                                                            
         ZIC   R4,BDISRS2                                                       
         SRL   R4,4                                                             
         MVC   P(L'MNEMOND),MNEMOND                                             
         LA    R2,P+L'MNEMOND                                                   
         EDIT  (R6),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R7),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C'('                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R4),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
****************** NEEDED A CONDITIONAL STATEMENT                               
         MVI   P,C' '                                                           
         MVC   P+1,P                                                            
         MVC   MNEOUT(L'MNEMOND),MNEMOND  ****REPEATED                          
         LA    R2,TEXT                    ****REPEATED                          
         MVI   0(R2),C'R'                                                       
         LA    R2,1(,R2)                                                        
         GOTO1 =V(HEXOUT),DMCB,REGMASK,(R2),1                                   
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R7),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
         MVI   0(R2),C'('                                                       
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C'R'                                                       
         LA    R2,1(,R2)                                                        
         GOTO1 =V(HEXOUT),DMCB,BDISRS2,(R2),1                                   
         LA    R2,1(,R2)                                                        
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
***********************************************************                     
*          DATA SET SCHODASM   AT LEVEL 091 AS OF 09/22/98                      
***********************************************************                     
*                                                                               
TYPE4RT  NTR1                                                                   
         MVC   RR,0(R5)                                                         
         MVC   MNEOUT,MNEMOND                                                   
         ZIC   R2,REGSRR                                                        
         SRL   R2,4            R2 NOW HAS THE FIRST REGISTER OPERAND            
         ZIC   R3,REGSRR                                                        
         SLL   R3,28                                                            
         SRL   R3,28           R3 NOW HAS THE SECOND REGISTER OPERAND           
         LA    R7,TEXT                                                          
         MVI   0(R7),C'R'                                                       
         LA    R7,1(R7)                                                         
         STC   R2,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R7),LOWER                                                    
         LA    R7,1(R7)                                                         
         MVI   0(R7),C','                                                       
         MVI   1(R7),C'R'                                                       
         LA    R7,2(R7)                                                         
         STC   R3,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R7),LOWER                                                    
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
*          DATA SET JBASMAIN   AT LEVEL 145 AS OF 09/22/98                      
TYPE5RT  NTR1                      INSTRUCTION IS AN SI INSTRUCTION             
         MVC   SI,0(R5)                                                         
         MVC   MNEOUT(L'MNEMOND),MNEMOND                                        
         LA    R2,TEXT                                                          
*                                                                               
         SR    RE,RE               PUT DISPLACEMENT INTO PRT.LINE               
         ICM   RE,3,BDISPSI                                                     
         SLL   RE,20                                                            
         SRL   RE,20                                                            
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
*                                  PUT BASE REG. INTO PRINT LINE                
         GOTO1 =V(HEXOUT),DMCB,BDISPSI,(R2),1                                   
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),C')'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RE,IMMED            PUT IMMED.OPERAND INTO PRINT LINE            
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
TYPE6RT  NTR1                                                                   
         MVC   SS1,0(R5)                                                        
         MVC   MNEOUT,MNEMOND                                                   
         ZIC   R2,LENGTH       R2 NOW HAS THE LENGTH                            
         LA    R2,1(R2)        ADJUST LENGTH                                    
         ICM   R3,3,BDISS1A                                                     
         SLL   R3,20                                                            
         SRL   R3,20           R3 NOW HAS THE FIRST DISPLACEMENT                
         ICM   R4,3,BDISS1A                                                     
         SRL   R4,12           R4 NOW HAS THE FIRST BASE                        
         ICM   R6,3,BDISS1B                                                     
         SLL   R6,20                                                            
         SRL   R6,20           R6 NOW HAS THE SECOND DISPLACEMENT               
         LA    R7,TEXT                                                          
         EDIT  (R3),(4,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),C'('                                                       
         LA    R7,1(R7)                                                         
         EDIT  (R2),(4,(R7)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R7,R0                                                            
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         MVI   0(R7),C'R'                                                       
         LA    R7,1(R7)                                                         
         STC   R4,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R7),LOWER                                                    
         LA    R7,1(R7)                                                         
         MVI   0(R7),C')'                                                       
         MVI   1(R7),C','                                                       
         LA    R7,2(R7)                                                         
         EDIT  (R6),(4,(R7)),,ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R7,R0                                                            
         MVI   0(R7),C'('                                                       
         LA    R7,1(R7)                                                         
         MVI   0(R7),C'R'                                                       
         LA    R7,1(R7)                                                         
         ICM   R6,3,BDISS1B                                                     
         SRL   R6,12                                                            
         STC   R6,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R7),LOWER                                                    
         LA    R7,1(R7)                                                         
         MVI   0(R7),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*****                                                                           
*                                                                               
*                                                                               
**********************************************************                      
*          DATA SET JBASMAI2   AT LEVEL 158 AS OF 09/21/98                      
**********************************************************                      
*                                                                               
*          DATA SET JBASMAI2   AT LEVEL 164 AS OF 09/22/98                      
TYPE7RT  NTR1                      INSTRUCTION IS AN SS INSTRUCTION             
         MVC   SS2,0(R5)                                                        
         MVC   MNEOUT(L'MNEMOND),MNEMOND                                        
         LA    R2,TEXT                                                          
*                                                                               
         SR    RE,RE               PUT 1ST DISPLACEMENT INTO PRINT LINE         
         ICM   RE,3,BDISS2A                                                     
         SLL   RE,20                                                            
         SRL   RE,20                                                            
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RE,LENGTHS          PUT 1ST LENGTH INTO PRINT LINE               
         SRL   RE,4                                                             
         LA    RE,1(RE)                                                         
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
*                                  PUT 1ST BASE REGISTER INTO PRT.LINE          
         GOTO1 =V(HEXOUT),DMCB,BDISS2A,(R2),1                                   
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),C')'                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         SR    RE,RE              PUT 2ND DISPLACEMENT INTO PRINT LINE          
         ICM   RE,3,BDISS2B                                                     
         SLL   RE,20                                                            
         SRL   RE,20                                                            
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         ZIC   RE,LENGTHS          PUT 2ND LENGTH INTO PRINT LINE               
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         LA    RE,1(RE)                                                         
         EDIT  (RE),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0                                                            
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
*                                  PUT 2ND BASE REGISTER INTO PRT.LINE          
         GOTO1 =V(HEXOUT),DMCB,BDISS2B,(R2),1                                   
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),C')'                                                       
*                                                                               
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
TYPE8RT  DS    0H                                                               
         NTR1                                                                   
*                                                                               
         XIT1                                                                   
*                                                                               
TYPE9RT  DS    0H                  INSTRUCTION IS AN RX INSTRUCTION             
         NTR1                                                                   
         MVC   RX,0(R5)                                                         
         MVC   MNEOUT,MNEMOND       MOVE MNEMONIC INTO PRINT LINE               
         LA    R2,TEXT                                                          
*                                                                               
         ZIC   RE,REGIN            MOVE SECOND BYTE OF INPUT INTO RE            
         SRL   RE,4                DERIVE MASK                                  
         EDIT  (RE),(4,(R2)),,ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R2,R0               MOVE POINTER ALONG P LINE                    
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BDISPRX                                                     
         SLL   RE,20                                                            
         SRL   RE,20                                                            
         EDIT  (RE),(4,(R2)),,ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R2,R0               MOVE PTR LENGTH OF OUTPUT                    
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P LINE                    
*                                                                               
         MVI   BYTE,X'00'                                                       
         MVN   BYTE,REGIN          MOVE LOW-ORDER NIBBLE OF INPUT'S             
*                                  SECOND BYTE (INDEX) INTO BYTE                
         SR    R0,R0               CLEAR REGISTER R0 AND                        
         MVI   0(R2),C'R'          PUT AN 'R' INTO THE INDEX                    
         ICM   R0,1,BYTE           MOVE THE INDEX INTO REGISTER 0               
         BNZ   NOTZERO                                                          
*                                                                               
         MVI   0(R2),C'0'          IF THE INDEX IS ZERO, ZERO OUT               
*                                                                               
NOTZERO2 DS    0H                  PUT INDEX INTO THE PRINT LINE                
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         STC   R0,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R2),LOWER       MOVE IN INDEX                                
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BDISPRX        MOVE SECOND BYTE OF INPUT INTO RE            
         SRL   RE,12                                                            
         STC   RE,BYTE                                                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,TWOBYTE,1                                   
         MVC   0(1,R2),LOWER       PUT BASE REG. INTO THE PRINT LINE            
         LA    R2,1(R2)            MOVE POINTER ALONG P                         
         MVI   0(R2),C')'                                                       
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
*                                                                               
TYPE10RT DS   0H                                                                
*        NTR1                                                                   
*        MVC   RS3,0(R5)                                                        
*        MVC   P(L'RS3),RS3                                                     
*        ZIC   R6,REGRS3                                                        
*        SRL   R6,4                                                             
*        IC    R4,BSHIFT                                                        
*        SLL   R4,26                                                            
*        SRL   R4,26                                                            
*        MVC   P(L'MNEMOND),MNEMOND                                             
*        LA    R2,P+L'MNEMOND                                                   
*        EDIT  (R6),(4,(R2)),ALIGN=LEFT                                         
*        AR    R2,R0                                                            
*        MVI   0(R2),C','                                                       
*        LA    R2,1(,R2)                                                        
*        EDIT  (R4),(4,(R2)),ALIGN=LEFT                                         
*        GOTO1 =V(PRINTER)                                                      
****************** NEEDED A CONDITIONAL STATEMENT                               
*        MVC   P(L'MNEMOND),MNEMOND  ****REPEATED                               
*        LA    R2,P+L'MNEMOND        ****REPEATED                               
*        MVI   0(R2),C'R'                                                       
*        LA    R2,1(,R2)                                                        
*        GOTO1 =V(HEXOUT),DMCB,REGRS3,(R2),1                                    
*        LA    R2,1(,R2)                                                        
*        MVI   0(R2),C','                                                       
*        LA    R2,1(,R2)                                                        
*        EDIT  (R4),(4,(R2)),ALIGN=LEFT                                         
*        GOTO1 =V(PRINTER)                                                      
*        XIT1                                                                   
*                                                                               
*****************************************************************               
* /////////////////////////////////////////////////////////////                 
*          END OF INTERNAL SUBROUTINES                                          
* /////////////////////////////////////////////////////////////                 
*****************************************************************               
*                                                                               
         LTORG                                                                  
VARS     DS    0D                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
BUFLEN   DS    F                                                                
LOCATION DS    H                                                                
BUFCODE  DS    H                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
*                                                                               
TWOBYTE  DS    0CL2                                                             
UPPER    DS    C                                                                
LOWER    DS    C                                                                
*                                                                               
DCTEXT   DS    0CL12                                                            
         DC    C'DC'                                                            
         DS    CL4                                                              
*        DC    C'''                                                             
DCDATA   DS    CL4                                                              
*        DC    C'''                                                             
*                                                                               
PANTEST  DC    C'PANNAME='                                                      
CNAME    DS    CL8                                                              
PANNAME  DS    CL10                                                             
WORK     DS    CL17                                                             
CARD     DS    CL80                                                             
*                                                                               
*                                                                               
*          DATA SET MZEIDIS    AT LEVEL 037 AS OF 09/18/98                      
FORMAT   DS    0CL36                                                            
         ORG   FORMAT                                                           
RX       DS    0CL4                   FORMAT1                                   
OPCRX    DS    X                                                                
REGIN    DS    X                                                                
BDISPRX  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
RS1      DS    0CL4                   FORMAT2                                   
OPCRS1   DS    X                                                                
REGS2    DS    X                                                                
BDISRS1  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
RS2      DS    0CL4                   FORMAT3                                   
OPCRS2   DS    X                                                                
REGMASK  DS    X                                                                
BDISRS2  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
RR       DS    0XL2                   FORMAT4                                   
OPCRR    DS    X                                                                
REGSRR   DS    X                                                                
         ORG                                                                    
         ORG   FORMAT                                                           
SI       DS    0XL4                   FORMAT5                                   
OPCSI    DS    X                                                                
IMMED    DS    X                                                                
BDISPSI  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
SS1      DS    0XL6                   FORMAT6                                   
OPCSS1   DS    X                                                                
LENGTH   DS    X                                                                
BDISS1A  DS    XL2                                                              
BDISS1B  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
SS2      DS    0XL6                    FORMAT7                                  
OPCSS2   DS    X                                                                
LENGTHS  DS    X                                                                
BDISS2A  DS    XL2                                                              
BDISS2B  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
SS3      DS    0XL6                    FORMAT8                                  
OPCSS3   DS    X                                                                
REGSSS3  DS    X                                                                
BDISS3A  DS    XL2                                                              
BDISS3B  DS    XL2                                                              
         ORG                                                                    
         ORG   FORMAT                                                           
RS3      DS    0CL4                    FORMAT9                                  
OPCRS3   DS    X                                                                
REGRS3   DS    X                       SECOND REGISTER IS ZERO                  
BDISRS3  DS    XL2                                                              
         ORG                                                                    
*                                                                               
EMPTY    DS    CL20                                                             
*                                                                               
******************************************************                          
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(SAMPLE)                                                        
         DC    X'80'                                                            
         DC    VL3(DUMMY)                                                       
*                                                                               
**FFER   DC     X'4120B1051A34D223C123B21492042008950190064780B0C890ECD+        
               00CBF01BFA2F307BFC6BF98'                                         
*                                                                               
TYPETAB  DS    0CL8                                                             
*              OPCODE,MNEMONIC,FORMAT                                           
*                                                                               
         DC    XL2'05',CL5'BALR',X'4'                                           
         DC    XL2'12',CL5'LTR',X'4'                                            
         DC    XL2'15',CL5'CLR',X'4'                                            
         DC    XL2'18',CL5'LR',X'4'                                             
         DC    XL2'40',CL5'STH',X'1'                                            
         DC    XL2'41',CL5'LA',X'1'                                             
         DC    XL2'42',CL5'STC',X'1'                                            
         DC    XL2'43',CL5'IC',X'1'                                             
         DC    XL2'47',CL5'BC',X'9'                                             
         DC    XL2'48',CL5'LH',X'1'                                             
         DC    XL2'50',CL5'ST',X'1'                                             
         DC    XL2'58',CL5'L',X'1'                                              
         DC    XL2'5A',CL5'A',X'1'                                              
         DC    XL2'5B',CL5'S',X'1'                                              
         DC    XL2'90',CL5'STM',X'2'                                            
         DC    XL2'92',CL5'MVI',X'5'                                            
         DC    XL2'98',CL5'LM',X'2'                                             
         DC    XL2'BE',CL5'STCM',X'3'                                           
         DC    XL2'BF',CL5'ICM',X'3'                                            
         DC    XL2'D2',CL5'MVC',X'6'                                            
         DC    XL2'4A',CL5'AH',X'1'                                             
         DC    XL2'1A',CL5'AR',X'4'                                             
         DC    XL2'4B',CL5'SH',X'1'                                             
         DC    XL2'1B',CL5'SR',X'4'                                             
         DC    XL2'5C',CL5'M',X'1'                                              
         DC    XL2'4C',CL5'MH',X'1'                                             
         DC    XL2'1C',CL5'MR',X'4'                                             
         DC    XL2'5D',CL5'D',X'1'                                              
         DC    XL2'1D',CL5'DR',X'4'                                             
         DC    XL2'59',CL5'C',X'1'                                              
         DC    XL2'49',CL5'CH',X'1'                                             
         DC    XL2'D5',CL5'CLC',X'6'                                            
         DC    XL2'95',CL5'CLI',X'5'                                            
         DC    XL2'19',CL5'CR',X'5'                                             
         DC    XL2'BD',CL5'CLM',X'3'                                            
         DC    XL2'54',CL5'N',X'1'                                              
         DC    XL2'94',CL5'NI',X'5'                                             
         DC    XL2'D4',CL5'NC',X'6'                                             
         DC    XL2'56',CL5'O',X'1'                                              
         DC    XL2'96',CL5'OI',X'5'                                             
         DC    XL2'D6',CL5'OC',X'6'                                             
         DC    XL2'17',CL5'XR',X'4'                                             
         DC    XL2'D7',CL5'XC',X'6'                                             
         DC    XL2'91',CL5'TM',X'5'                                             
         DC    XL2'47',CL5'BC',X'1'                                             
         DC    XL2'4D',CL5'BAS',X'1'                                            
         DC    XL2'0D',CL5'BASR',X'4'                                           
         DC    XL2'46',CL5'BCT',X'1'                                            
         DC    XL2'06',CL5'BCTR',X'4'                                           
         DC    XL2'F2',CL5'PACK',X'7'                                           
         DC    XL2'F3',CL5'UNPK',X'7'                                           
         DC    XL2'4F',CL5'CVB',X'1'                                            
         DC    XL2'4E',CL5'CVD',X'1'                                            
         DC    XL2'44',CL5'EX',X'1'                                             
         DC    XL2'13',CL5'LCR',X'4'                                            
         DC    XL2'10',CL5'LPR',X'4'                                            
         DC    XL2'11',CL5'LNR',X'4'                                            
         DC    XL2'D1',CL5'MVN',X'6'                                            
         DC    XL2'D3',CL5'MVZ',X'6'                                            
         DC    XL2'0E',CL5'MVCL',X'4'                                           
         DC    XL2'FA',CL5'AP',X'7'                                             
         DC    XL2'F8',CL5'ZAP',X'7'                                            
         DC    XL2'FB',CL5'SP',X'7'                                             
         DC    XL2'FC',CL5'MP',X'7'                                             
         DC    XL2'FD',CL5'DP',X'7'                                             
         DC    XL2'0F',CL5'CLCL',X'4'                                           
         DC    XL2'F9',CL5'CP',X'7'                                             
         DC    XL2'8B',CL5'SLA',X'9'                                            
         DC    XL2'8F',CL5'SLDA',X'9'                                           
         DC    XL2'89',CL5'SLL',X'9'                                            
         DC    XL2'8D',CL5'SLDL',X'9'                                           
         DC    XL2'8A',CL5'SRA',X'9'                                            
         DC    XL2'8E',CL5'SRDA',X'9'                                           
         DC    XL2'88',CL5'SRL',X'9'                                            
         DC    XL2'8C',CL5'SRDL',X'9'                                           
         DC    XL2'86',CL5'BXH',X'2'                                            
         DC    XL2'87',CL5'BXLE',X'2'                                           
         DC    XL2'DC',CL5'TR',X'6'                                             
         DC    XL2'DD',CL5'TRT',X'6'                                            
         DC    X'FF'                                                            
*                                                                               
BUFFER   DS    50000C                                                           
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         ORG    P                                                               
LOCOUT   DS     CL4                                                             
         DS     CL6                                                             
MNEOUT   DS     CL5                                                             
         DS     CL3                                                             
TEXT     DS     CL10                                                            
         ORG                                                                    
*                                                                               
CODESD   DSECT                                                                  
CODED    DS    XL2                                                              
MNEMOND  DS    CL5                                                              
FORMATD  DS    X                                                                
CODESLQ  EQU  *-CODESD                                                          
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'254MZEIMAIN  09/29/98'                                      
         END                                                                    
