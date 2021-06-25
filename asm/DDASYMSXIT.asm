*          DATA SET DDASYMSXIT AT LEVEL 007 AS OF 12/16/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046251.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PROCESS USING(WARN(15))                                                        
***********************************************************************         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                     THIS MODULE IS OBSOLETE                                   
*                                                                               
*                 It has been replaced by DDBLDSYMS.                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*PHASE DFSYMXTA                                                                 
***********************************************************************         
*                                                                     *         
* This is a DFSORT user exit. It reads a SYSADATA file produced by    *         
* the assembler (typically from a DSECT member). This program         *         
* transforms the symbol records in the SYSADATA file into DFSORT      *         
* symbol records. This obviates the need for us to maintain two       *         
* copies of a record layout (one as an assembler DSECT, the other     *         
* in DFSORT symbol definition form). The idea is that by using this   *         
* utility, we only need to maintain the assembler DSECT, and we       *         
* generate its equivalent DFSORT symbols programatically. This has    *         
* the added advantage that a PANSCAN will find hits on the same       *         
* symbols, whether they are in assembler source or DFSORT source.     *         
*                                                                     *         
* Using this utility requires an understanding of its limitations.    *         
* DS/DC statement translation is relatively straightforward. We can   *         
* use the location counter, length attribute, and type attribute to   *         
* construct the equivalent DFSORT symbol. The exception is when we    *         
* need to construct a DFSORT symbol without a length (which is        *         
* DFSORT's syntax indicating that the symbol is variable-length).     *         
* In that case, we use the little-known "program type" attribute. If  *         
* the program type attribute is set to C'LEN0', this is taken to      *         
* mean that we must generate a variable-length DFSORT symbol.         *         
*                                                                     *         
* For example, the following DS statements:                           *         
*                                                                     *         
*  000000   FIXEDREC DS    CL256                                      *         
*                    ORG   FIXEDREC                                   *         
*  000000   RDW      DS    XL4                                        *         
*  000004   VARREC   DS    0CP(C'LEN0')                               *         
*                                                                     *         
* are translated into the following DFSORT symbol statements:         *         
*                                                                     *         
*  FIXEDREC,1,256,CH                                                  *         
*  RDW,1,4,BI                                                         *         
*  VARREC,5                                                           *         
*                                                                     *         
* EQU statements are challenging. We don't know anything about the    *         
* original EQU statements other than what appears in the SYSADATA     *         
* file. A typical EQU statement (without operands 2 and/or 3) will    *         
* cause the assembler to assign a length attribute of 1 and a type    *         
* attribute of 'U' to the equated symbol. This means that in order    *         
* for this utility to generate appropriate and useful DFSORT symbols  *         
* for the equates, the programmer must sometimes provide operands     *         
* 2 and/or 3 in the EQU statement. In particular, if a DFSORT         *         
* constant must be specified as character or hex, then the type       *         
* attribute must be specified in operand 3 of the EQU statement.      *         
*                                                                     *         
* The rules are as follows. If the type attribute is:                 *         
*  'U': the entire EQU value is edited in signed decimal format.      *         
*  'C' OR 'X': the length attribute determines the number of bytes to *         
*       be extracted from the 4-byte EQU value. The bytes are assumed *         
*       to be right-aligned within the location counter field. The    *         
*       type attribute determines the format of the DFSORT constant.  *         
*                                                                     *         
* For example, the following EQU statements:                          *         
*                                                                     *         
*  DECIMALA EQU   C'A'                                                *         
*  LETTERA  EQU   C'A',,C'C'                                          *         
*  HEXLETA  EQU   X'C1',,C'X'                                         *         
*  ONE93    EQU   193                                                 *         
*  CHARS4   EQU   C'ABCD',4,C'C'                                      *         
*  HEX4     EQU   X'FFFEFDFC',4,C'X'                                  *         
*  BIGNUM   EQU   999999999                                           *         
*  ZERO     EQU   0                                                   *         
*  MINUS1   EQU   -1                                                  *         
*  DSECTLQ  EQU   *-DSECT                                             *         
*                                                                     *         
* are translated into the following DFSORT symbol statements:         *         
*                                                                     *         
*  DECIMALA,193                                                       *         
*  LETTERA,C'A'                                                       *         
*  HEXLETA,X'C1'                                                      *         
*  ONE93,193                                                          *         
*  CHARS4,C'ABCD'                                                     *         
*  HEX4,X'FFFEFDFC'                                                   *         
*  BIGNUM,999999999                                                   *         
*  ZERO,0                                                             *         
*  MINUS1,-1                                                          *         
*  DSECTLQ,<whatever the resolved value is as an integer>             *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE DDASMADATA                                                     
         EJECT                                                                  
DESYMXIT CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
*                                                                               
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
*                                                                               
         L     R3,0(R1)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         MVI   SYMCARD,C' '        PREFILL SYMBOL STATEMENT WITH BLANKS         
         MVC   SYMCARD+1(L'SYMCARD-1),SYMCARD                                   
*                                                                               
         USING ASMADATA,R3                                                      
         ICM   RE,15,ADSYM_NAME_OFF   SOFT OFFSET TO SYMBOL NAME                
         LA    RE,4(R3,RE)            RE = A(SYMBOL NAME)                       
         ICM   RF,15,ADSYM_NAME_LEN   SYMBOL NAME LENGTH                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SYMCARD(0),0(RE)    BEGIN STATEMENT WITH SYMBOL NAME             
         LA    R4,SYMCARD+1(RF)                                                 
         MVI   0(R4),C','          DELIMIT WITH A COMMA                         
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   ADSYM_TYPE,ADSYM_TYPE_ORDINARY  ORDINARY LABEL?                  
         BNE   PROCEQU             NO: EQU                                      
*                                                                               
* This is a DS/DC statement.                                                    
*                                                                               
         L     RF,ADSYM_LOCTR      LOCATION COUNTER (DISPLACEMENT)...           
         AHI   RF,1                ...BUT DFSORT USES COLUMN NUMBERS!           
         EDIT  (RF),(8,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R4,R0               R4 = A(DELIMITER), IF NEEDED                 
*                                                                               
* 1. If the program type is C'LEN0', then this is a special signal to           
*     this utility that the DFSORT symbol is to be generated without            
*     a length or type attribute. This is necessary to force DFSORT to          
*     recognize the symbol as variable-length. Otherwise:                       
* 2. If the duplication factor is zero, use the assembler's length              
*     attribute as the DFSORT symbol length attribute.                          
* 3. If the duplication factor is non-zero, then multiply it by the             
*     length attribute to derive the DFSORT symbol length attribute.            
*                                                                               
         CLC   ADSYM_PROGRAM_TYPE,=C'LEN0'  NO LENGTH ATTRIBUTE WANTED?         
         BE    KEEP                         CORRECT                             
*                                                                               
         MVI   0(R4),C','          DELIMITER BETWEEN COLUMN AND LENGTH          
         LA    R4,1(R4)                                                         
         ICM   RF,15,ADSYM_BYTE_LEN  LENGTH ATTRIBUTE                           
         ICM   R0,15,ADSYM_DUP       DUPLICATION FACTOR                         
         BZ    *+6                                                              
         MR    RE,R0               RESULT IS IN RF                              
         EDIT  (RF),(8,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R4,R0                                                            
         MVI   0(R4),C','          DELIMIT WITH A COMMA                         
         LA    R4,1(R4)                                                         
*                                                                               
         LA    RF,DTYPETAB         DATATYPE TRANSLATION TABLE                   
         CLI   0(RF),X'FF'                                                      
         BE    KEEP                UNKNOWN DATATYPE: IGNORE                     
         CLC   ADSYM_ATTR,0(RF)    TYPE ATTRIBUTE                               
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     *-14                                                             
         MVC   0(2,R4),1(RF)       DFSORT DATATYPE                              
         CLC   =C'??',1(RF)        UNKNOWN TRANSLATION VALUE?                   
         BNE   *+10                                                             
         MVC   0(1,R4),ADSYM_ATTR  SHOW WHICH ONE NEEDS EXAMINATION             
         B     KEEP                                                             
         EJECT                                                                  
PROCEQU  DS    0H                                                               
*                                                                               
* NOTE: In EQU symbol statements, the location counter field contains           
*       the 4-byte value of the equate. Numeric equates are always              
*       treated as 4-byte values (i.e., the value is right-justified).          
*                                                                               
         CLI   ADSYM_ATTR,C'U'     TYPE ATTRIBUTE EXPLICITLY PROVIDED?          
         BNE   CHKCHAR             YES                                          
         EDIT  ADSYM_LOCTR,(17,0(R4)),ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-           
         B     KEEP                CONVERT ALL 4 BYTES INTO SIGNED DEC.         
*                                                                               
CHKCHAR  DS    0H                                                               
         CLI   ADSYM_ATTR,C'C'     TYPE ATTRIBUTE IS CHARACTER?                 
         BNE   CHKHEX              NO: MAYBE IT'S HEX                           
*                                                                               
         MVC   0(2,R4),=C'C'''                                                  
         LA    R4,2(R4)            BUMP TO START OF VALUE                       
*                                                                               
         ICM   RF,15,ADSYM_BYTE_LEN  LENGTH ATTRIBUTE                           
         BCTR  RF,0                                                             
         LA    R5,ADSYM_LOCTR+L'ADSYM_LOCTR-1 A(LAST BYTE OF EQU VALUE)         
         SR    R5,RF               BACK UP TO FIRST BYTE OF VALUE               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)       MOVE IN ENTIRE EQUATED VALUE                 
         LA    R4,1(RF,R4)         BUMP PAST END OF OUTPUT STRING               
         MVI   0(R4),C''''         DELIMIT THE SYMBOL                           
         B     KEEP                                                             
*                                                                               
CHKHEX   DS    0H                                                               
         CLI   ADSYM_ATTR,C'X'     TYPE ATTRIBUTE IS HEX?                       
         BE    *+12                                                             
         MVI   0(R4),C'?'          CAUSE DELIBERATE SYNTAX ERROR...             
         B     KEEP                ...UNTIL WE FIGURE OUT HOW TO HANDLE         
*                                                                               
         MVC   0(2,R4),=C'X'''                                                  
         LA    R4,2(R4)            BUMP TO START OF VALUE                       
*                                                                               
         ICM   R2,15,ADSYM_BYTE_LEN   LENGTH ATTRIBUTE                          
         BP    *+6                                                              
         DC    H'0'                MUST BE POSITIVE!                            
         LA    R5,ADSYM_LOCTR+L'ADSYM_LOCTR  A(JUST PAST EQUATED VALUE)         
         SR    R5,R2               BACK UP TO FIRST BYTE OF VALUE               
*                                                                               
DO1HEX   DS    0H                                                               
         IC    R0,0(R5)            R0=NEXT INPUT HEX CHR                        
         SRDL  R0,4                                                             
         SRL   R1,28               R1=LOB OF INPUT CHR                          
         IC    R1,HEXOUTC(R1)                                                   
         STC   R1,1(R4)                                                         
         SRDL  R0,4                                                             
         SRL   R1,28               R1=HOB OF INPUT CHR                          
         IC    R1,HEXOUTC(R1)                                                   
         STC   R1,0(R4)                                                         
         LA    R5,1(R5)            BUMP TO NEXT INPUT HEX CHR                   
         LA    R4,2(R4)                                                         
         JCT   R2,DO1HEX                                                        
*                                                                               
         MVI   0(R4),C''''         DELIMIT THE SYMBOL                           
         DROP  R3                                                               
*                                                                               
KEEP     DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LA    R1,SYMRDW           R1: A(CONSTRUCTED SYMBOL STATEMENT)          
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
DTYPETAB DS    0D                  DATATYPE TRANSLATION TABLE                   
*                                                                               
* Translate the 1-character assembler type attribute (T') to its                
* equivalent 2-character DFSORT data format. If we don't know how to            
* translate a particular datatype, we generate "??", which will cause           
* DFSORT to fail. Then we can investigate further.                              
*                                                                               
         DC    C'A',C'BI'          ADDRESS CONSTANT                             
         DC    C'B',C'BI'          UNSIGNED BINARY                              
         DC    C'C',C'CH'          CHARACTER                                    
         DC    C'D',C'BI'          FLOATING POINT, BUT AT DDS, BINARY!          
         DC    C'E',C'??'          FLOATING POINT                               
         DC    C'F',C'FI'          SIGNED BINARY                                
         DC    C'G',C'FI'          SIGNED BINARY                                
         DC    C'H',C'FI'          SIGNED BINARY                                
         DC    C'I',C'??'                                                       
         DC    C'J',C'??'                                                       
         DC    C'K',C'??'          FLOATING POINT                               
         DC    C'L',C'??'          FLOATING POINT                               
         DC    C'M',C'??'                                                       
         DC    C'N',C'??'                                                       
         DC    C'O',C'??'                                                       
         DC    C'P',C'PD'          PACKED DECIMAL                               
         DC    C'Q',C'??'          ADDRESS CONSTANT                             
         DC    C'R',C'BI'          ADDRESS CONSTANT                             
         DC    C'S',C'??'          ADDRESS CONSTANT                             
         DC    C'T',C'??'                                                       
         DC    C'U',C'??'                                                       
         DC    C'V',C'??'          ADDRESS CONSTANT                             
         DC    C'W',C'??'                                                       
         DC    C'X',C'BI'          UNSIGNED BINARY                              
         DC    C'Y',C'??'          ADDRESS CONSTANT                             
         DC    C'Z',C'ZD'          ZONED DECIMAL                                
         DC    C'@',C'??'          GRAPHIC CHARACTER                            
         DC    X'FF'                                                            
*                                                                               
HEXOUTC  DC    C'0123456789ABCDEF' TRANSLATION CHARACTERS FOR HEXOUT            
         SPACE 2                                                                
         ORG   DESYMXIT+(((*-DESYMXIT)/256)+1)*256 FOR I-CACHE PIPELINE         
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
         SPACE 3                                                                
DUB      DS    D                                                                
WORK     DS    CL17                                                             
*                                                                               
SYMRDW   DC    H'80'               BUILD SYMBOL STATEMENT HERE                  
         DC    H'0'                                                             
SYMCARD  DS    CL80                SYMBOL STATEMENT                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDASYMSXIT12/16/15'                                      
         END                                                                    
