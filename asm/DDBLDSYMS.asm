*          DATA SET DDBLDSYMS  AT LEVEL 006 AS OF 02/12/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE BLDSYMSA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*            UTILITY TO GENERATE DFSORT/ICETOOL SYMBOLS               *         
*                      FROM AN ASSEMBLER DSECT                        *         
*                                                                     *         
* This program reads a SYSADATA file produced by the assembler        *         
* (typically the source contains one or more DSECTs). The program     *         
* transforms the symbol records in the SYSADATA file into DFSORT      *         
* symbol records. This obviates the need for us to maintain two       *         
* copies of a record layout (one as an assembler DSECT, the other     *         
* in DFSORT symbol definition form). The idea is that by using this   *         
* utility, we only need to maintain the assembler DSECT, and we can   *         
* generate its equivalent DFSORT symbols programmatically.            *         
*                                                                     *         
* DDNAME SYMSOUT is the output dataset containing the generated       *         
* symbols.                                                            *         
*                                                                     *         
* =====================> VERY IMPORTANT <============================ *         
*                                                                     *         
* It is certainly possible to generate a *catalogued* SYMSOUT dataset *         
* which can be referenced by production DFSORT jobs. However, that    *         
* isn't what we usually do, because that would require us to remember *         
* to regenerate new DFSORT symbols each time an associated DSECT      *         
* member is changed.                                                  *         
*                                                                     *         
* Instead of cataloguing the SYMSOUT dataset, we typically generate   *         
* the DFSORT symbols **DYNAMICALLY** within our production jobs.      *         
* I.e., we execute this utility at the start of a production job, and *         
* write SYMSOUT to a *temporary* dataset. We then pass that dataset   *         
* to the subsequent ICETOOL (or DFSORT) job step(s) which require     *         
* the symbols.                                                        *         
*                                                                     *         
* Why does this matter?                                               *         
*                                                                     *         
* WE ARE USED TO THINKING THAT CHANGES TO AN ASSEMBLER DSECT DON'T    *         
* HAVE ANY EFFECT UNTIL SOMETHING THAT INCLUDES IT IS RELINKED. THAT  *         
* IS NOT NECESSARILY TRUE WHERE THIS UTILITY IS CONCERNED. IT DEPENDS *         
* WHAT THE JCL IS DOING. IF THIS UTILITY WRITES ITS OUTPUT TO A       *         
* TEMPORARY DATASET WHICH IS PASSED TO A SUBSEQUENT STEP (WHICH IS    *         
* TYPICALLY WHAT WE DO), THEN ANY CHANGES MADE TO THE DSECT WILL TAKE *         
* EFFECT THE INSTANT THE DSECT MEMBER IS PROMOTED TO THE PRODUCTION   *         
* LIBRARY !!!                                                         *         
*                                                                     *         
* =================================================================== *         
*                                                                     *         
* THE DEVIL IS IN THE DETAILS:                                        *         
*                                                                     *         
* Using this utility requires an understanding of its limitations.    *         
* The only assembler statements we examine are DS, DC, and EQU.       *         
*                                                                     *         
* DS/DC statement translation is relatively straightforward. We can   *         
* use the location counter, length attribute, and type attribute to   *         
* construct the equivalent DFSORT symbol. Note that in addition to    *         
* the original symbol, we also generate an additional symbol denoting *         
* the field length, and in the case of variable-length fields, a      *         
* variable-length symbol is generated.                                *         
*                                                                     *         
* For example, the following DS statements:                           *         
*                                                                     *         
*  000000   FIXEDREC DS    CL256                                      *         
*                    ORG   FIXEDREC                                   *         
*  000000   RDW      DS    XL4                                        *         
*  000004   VARREC   DS    0C                                         *         
*                                                                     *         
* are translated into the following DFSORT symbol statements:         *         
*                                                                     *         
*  FIXEDREC,1,256,CH                                                  *         
*  LEN_FIXEDREC,256                                                   *         
*  RDW,1,4,BI                                                         *         
*  LEN_RDW,4                                                          *         
*  VARREC,5,1,CH                                                      *         
*  VARREC_VL,5                                                        *         
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
* Note: If the SYSIN parameter card "ADDRDW=Y" is provided, then      *         
*       all calculated column numbers will be incremented by four.    *         
*       This is useful when a converted DSECT represents a variable-  *         
*       length record, but the RDW is not in the DSECT itself. In     *         
*       this case these additional symbol statements are generated:   *         
*         RDW,1,4,BI                                                  *         
*         LEN_RDW,4                                                   *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE DDASMADATA                                                     
         EJECT                                                                  
BLDSYMS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*BLDSYMS,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE,=CL60'*** ERROR REPORT ***'                                
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE10'  ***NOTE P2***                      
         CLC   =C'/*',CARD         EOF (OR NO SYSIN FILE PRESENT)?              
         JE    INITSORT            YES                                          
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         JE    INITSORT            YES                                          
*                                                                               
         CLC   =C'ADDRDW=Y',CARD   RDW IS PRESENT, BUT NOT IN DSECT?            
         JNE   INITSORT                                                         
         MVI   ADDRDW,C'Y'         CORRECT: ADD 4 TO ALL COLUMN NUMBERS         
*                                                                               
INITSORT DS    0H                                                               
         LHI   RE,SORTRECQ         INITIALIZE SORTER PARAMETER CARDS            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB                                                
         LHI   RE,L'SYMCARD+1                                                   
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB                                               
         LHI   RE,SORTKEYQ                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+17(3),DUB                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,(X'80',RECCARD),(X'80',0)               
*                                                                               
         OPEN  SYSADATA                                                         
*                                                                               
         CLI   ADDRDW,C'Y'         ADDRDW=Y CARD WAS PASSED?                    
         JNE   NEXTREC                                                          
*                                                                               
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         MVI   SYMSORT,0           YES: GENERATE SYMBOLS FOR RDW                
         MVC   SYMSTMT#,=X'00000000'   (FORCE TO START OF SORT)                 
         MVC   SYMCARD(10),=C'RDW,1,4,BI'                                       
         MVC   SYMCARD+61(9),=C' ADDRDW=Y'                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         MVI   SYMSORT,X'FF'       FORCE THIS TO THE END OF SYMNAMES            
         MVC   SYMCARD(9),=C'LEN_RDW,4'                                         
         MVC   SYMCARD+61(10),=C' abs. len.'                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
NEXTREC  DS    0H                                                               
         GET   SYSADATA,IOAREA                                                  
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         USING ASMADATA,R3                                                      
         LA    R3,IOAREA                                                        
*                                                                               
         CLI   ADATA_VERSION,ADATA_VERASM  MUST BE ASSEMBLER SOURCE             
         JNE   NEXTREC                                                          
         CLI   ADATA_LEVEL,ADATA_L3        MUST BE ARCHITECTURE LEVEL 3         
         JNE   NEXTREC                                                          
         CLC   ADATA_RECTYPE,=AL2(ADATA_RECSYM)  ONLY KEEP SYMBOL RECS.         
         JE    CHKSYMB                                                          
*                                                                               
         CLC   SPCLCMNT,SPACES     ALREADY HAVE LEVEL STAMP?                    
         BNE   NEXTREC             YES: IGNORE SUBSEQUENT ONES                  
         CLC   ADATA_RECTYPE,=AL2(ADATA_RECSOURCE) LOOK FOR LEVEL STAMP         
         BNE   NEXTREC                                                          
         CLC   ADSRC_MEMBER_LEN,=F'0'                                           
         BNE   NEXTREC                                                          
         CLC   ADSRC_PARENT_LEN,=F'0'                                           
         BNE   NEXTREC                                                          
         CLC   ADSRC_RECORD_PAN_SPECIAL_COMMENT_DATA_SET,=C'*          +        
               DATA SET'                                                        
         BNE   NEXTREC                                                          
         CLC   ADSRC_RECORD_PAN_SPECIAL_COMMENT_AT_LEVEL,=C' AT LEVEL '         
         CLC   ADSRC_RECORD_PAN_SPECIAL_COMMENT_AS_OF,=C' AS OF '               
         BNE   NEXTREC                                                          
         CLC   ADSRC_RECORD_PAN_SPECIAL_COMMENT_BLANKS,SPACES                   
         BNE   NEXTREC                                                          
         ICM   R1,15,ADSRC_RECORD_LEN                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPCLCMNT(0),ADSRC_RECORD  SAVE LEVEL STAMP                       
         B     NEXTREC                                                          
*                                                                               
CHKSYMB  DS    0H                                                               
* NOTE: The maximum DFSORT symbol length is 50. Since we are                    
*       generating a symbol with prefix "LEN_" for each symbol, the             
*       effective maximum symbol length is 46 for DS/DC symbols.                
*                                                                               
         MVC   FULL,=A(SYMBNAME_MAXLEN)  ASSUME MAX SYMBOL LENGTH OF 50         
         CLI   ADSYM_TYPE,ADSYM_TYPE_EQU EQU SYMBOL?                            
         JE    PROCSYMB                  YES                                    
         MVC   FULL,=A(SYMBNAME_MAXLEN-4)  MUST LEAVE ROOM FOR "LEN_"           
         CLI   ADSYM_TYPE,ADSYM_TYPE_ORDINARY    (E.G., DS, DC)                 
         JNE   NEXTREC                   NOT EQU/DS/DC: SKIP IT                 
*                                                                               
PROCSYMB DS    0H                                                               
         ICM   R5,15,ADSYM_NAME_OFF   SOFT OFFSET TO SYMBOL NAME                
         LA    R5,4(R3,R5)            R5 = A(SYMBOL NAME)                       
         ICM   R6,15,ADSYM_NAME_LEN   SYMBOL NAME LENGTH                        
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         J     *+10                                                             
         MVC   P+43(0),0(R5)       SAVE DFSORT SYMBOL NAME IN "P"               
*                                                                               
         MVC   SYMSTMT#,ADSYM_STMT ASSEMBLER SOURCE STATEMENT NUMBER            
         MVC   SYMBNAME,SPACES     PREFILL SYMBOL NAME WITH BLANKS              
*                                                                               
         L     R0,FULL             MAXIMUM SYMBOL LENGTH                        
         BCTR  R0,0                (ADJUST FOR BCTR ABOVE)                      
         CR    R6,R0                                                            
         JNH   PROCSYM5                                                         
         MVC   P(43),=C'*** ERROR *** SYMBOL TOO LONG FOR DFSORT:  '            
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'8'       SET BAD RETURN CODE                          
         J     NEXTREC             SKIP THIS SYMBOL                             
*                                                                               
PROCSYM5 DS    0H                                                               
         EX    R6,*+8                                                           
         J     *+10                                                             
         MVC   SYMBNAME(0),0(R5)   DFSORT SYMBOL NAME                           
*                                                                               
         CLI   ADSYM_TYPE,ADSYM_TYPE_ORDINARY  ORDINARY LABEL?                  
         JNE   PROCEQU             NO: EQU                                      
*                                                                               
* This is a DS/DC statement.                                                    
*                                                                               
* We always generate two DFSORT symbols for each assembler DS/DC                
* symbol. One is the "regular" symbol with a length and datatype. The           
* other is either a length symbol (derived from the length attribute),          
* or a variable-length symbol (i.e., without a length).                         
*                                                                               
* 1.  If the duplication factor is non-zero, then multiply it by the            
*      length attribute to derive the DFSORT symbol length attribute.           
* 2a. If the duplication factor is zero, use the assembler's length             
*      attribute as the DFSORT symbol length attribute. **BUT** :               
* 2b. If the DS/DC field is defined as 0C, 0CL1, 0X, or 0XL1, then this         
*      is assumed to be the beginning of a variable-length field. In            
*      this case, in addition to the "regular" DFSORT symbol with a             
*      length, we also generate an additional symbol with neither a             
*      length nor a datatype, which is how DFSORT knows it is a                 
*      variable-length symbol.                                                  
* 3.  We do not support DS/DC statements containing more than one term.         
*      E.g., "DS 2XL3" is fine, but "DS XL3,XL3" is not.                        
*                                                                               
         L     RF,ADSYM_LOCTR      LOCATION COUNTER (DISPLACEMENT)...           
         AHI   RF,1                ...BUT DFSORT USES COLUMN NUMBERS!           
         CLI   ADDRDW,C'Y'         ADDRDW=Y CARD WAS PASSED?                    
         JNE   *+8                                                              
         AHI   RF,4                YES: ADD 4 FOR RDW TO COLUMN NUMBER          
         ST    RF,COLUMN#          SAVE THE COLUMN NUMBER                       
*                                                                               
         ICM   RF,15,ADSYM_BYTE_LEN  LENGTH ATTRIBUTE                           
         ICM   R0,15,ADSYM_DUP       DUPLICATION FACTOR                         
         JZ    *+6                                                              
         MR    RE,R0               RESULT IS IN RF                              
         ST    RF,LENGTH           SAVE THE FIELD LENGTH                        
*                                                                               
         LA    R1,DTYPETAB         DATATYPE TRANSLATION TABLE                   
         CLC   ADSYM_ATTR,0(R1)    TYPE ATTRIBUTE                               
         JE    CHKDUP                                                           
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         JNE   *-18                                                             
*                                                                               
         MVC   P(43),=C'*** ERROR *** UNSUPPORTED DATATYPE:        '            
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'8'       SET BAD RETURN CODE                          
         J     NEXTREC             SKIP THIS SYMBOL                             
*                                                                               
CHKDUP   DS    0H                                                               
         MVC   DATATYPE,1(R1)      DFSORT'S DATATYPE                            
*                                                                               
         CLC   ADSYM_DUP,=F'0'     DUPLICATION FACTOR OF ZERO ?                 
         JNE   LENSYMB                                                          
         CLI   ADSYM_ATTR,C'C'     YES: ALSO TYPE "C" OR "X" ?                  
         JE    *+12                                                             
         CLI   ADSYM_ATTR,C'X'                                                  
         JNE   LENSYMB                                                          
         CLC   ADSYM_BYTE_LEN,=F'1'  YES: LENGTH ATTRIBUTE = 1 ?                
         JNE   LENSYMB                                                          
*                                                                               
* Generate a variable-length symbol (i.e., without a length).                   
*                                                                               
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         ICM   RF,15,ADSYM_NAME_LEN  SYMBOL NAME LENGTH                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SYMCARD(0),SYMBNAME   DFSORT SYMBOL NAME                         
         LA    R4,SYMCARD+SYMBNAME_MAXLEN                                       
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         JE    *-6                 BACK UP TO LAST NON-BLANK                    
         MVC   1(4,R4),=C'_VL,'                                                 
         EDIT  COLUMN#,(8,5(R4)),ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   SYMCARD+61(9),=C' VL start'                                      
*                                                                               
         MVI   SYMSORT,0           FOR SORTING OF SYMNAMES OUTPUT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         J     ORDSYMB                                                          
*                                                                               
LENSYMB  DS    0H                                                               
*                                                                               
* For all other DS/DC symbols, generate a DFSORT symbol representing            
* the field length before generating the regular symbol.                        
*                                                                               
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         MVC   SYMCARD(4),=C'LEN_' CONSTANT PREFIX                              
         ICM   RF,15,ADSYM_NAME_LEN  SYMBOL NAME LENGTH                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SYMCARD+4(0),SYMBNAME DFSORT SYMBOL NAME                         
         LA    R4,SYMCARD+SYMBNAME_MAXLEN                                       
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         JE    *-6                 BACK UP TO LAST NON-BLANK                    
         MVI   1(R4),C','                                                       
         EDIT  LENGTH,(8,2(R4)),ALIGN=LEFT,ZERO=NOBLANK                         
         MVC   SYMCARD+61(10),=C' abs. len.'                                    
*                                                                               
         MVI   SYMSORT,X'FF'       FORCE THESE TO THE END OF SYMNAMES           
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
ORDSYMB  DS    0H                                                               
*                                                                               
* Generate a regular DFSORT symbol (i.e., with a column and length).            
*                                                                               
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         ICM   RF,15,ADSYM_NAME_LEN  SYMBOL NAME LENGTH                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   SYMCARD(0),SYMBNAME   DFSORT SYMBOL NAME                         
         LA    R4,SYMCARD+SYMBNAME_MAXLEN                                       
         BCTR  R4,0                                                             
         CLI   0(R4),C' '                                                       
         JE    *-6                 BACK UP TO LAST NON-BLANK                    
         MVI   1(R4),C','                                                       
         EDIT  COLUMN#,(8,2(R4)),ALIGN=LEFT,ZERO=NOBLANK                        
         AR    R4,R0                                                            
         AHI   R4,2                                                             
         MVI   0(R4),C','                                                       
         EDIT  LENGTH,(8,1(R4)),ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R4,R0                                                            
         MVI   1(R4),C','                                                       
         MVC   2(2,R4),DATATYPE                                                 
         MVC   SYMCARD+61(6),=C' DS/DC'                                         
*                                                                               
         MVI   SYMSORT,0           FOR SORTING OF SYMNAMES OUTPUT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         J     NEXTREC                                                          
         EJECT                                                                  
* Handle EQU statements.                                                        
*                                                                               
PROCEQU  DS    0H                                                               
         MVC   SYMCARD,SPACES      INITIALIZE THE SYMBOL STATEMENT              
         MVC   SYMCARD(L'SYMBNAME),SYMBNAME   SYMBOL NAME                       
         ICM   RF,15,ADSYM_NAME_LEN           SYMBOL NAME LENGTH                
         LA    R4,SYMCARD(RF)                                                   
         MVI   0(R4),C','          DELIMIT WITH A COMMA                         
         LA    R4,1(R4)                                                         
*                                                                               
* NOTE: In EQU symbol statements, the location counter field contains           
*       the 4-byte value of the equate. Numeric equates are always              
*       treated as 4-byte values (i.e., the value is right-justified).          
*                                                                               
         CLI   ADSYM_ATTR,C'U'     TYPE ATTRIBUTE EXPLICITLY PROVIDED?          
         JNE   CHKCHAR             YES                                          
         EDIT  ADSYM_LOCTR,(17,0(R4)),ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-           
         MVC   SYMCARD+61(4),=C' EQU'                                           
*                                                                               
*                                  CONVERT ALL 4 BYTES INTO SIGNED DEC.         
         MVI   SYMSORT,0           FOR SORTING OF SYMNAMES OUTPUT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         J     NEXTREC                                                          
*                                                                               
CHKCHAR  DS    0H                                                               
         CLI   ADSYM_ATTR,C'C'     TYPE ATTRIBUTE IS CHARACTER?                 
         JNE   CHKHEX              NO: MAYBE IT'S HEX                           
*                                                                               
         MVC   0(2,R4),=C'C'''                                                  
         LA    R4,2(R4)            BUMP TO START OF VALUE                       
*                                                                               
         ICM   RF,15,ADSYM_BYTE_LEN  LENGTH ATTRIBUTE                           
         BCTR  RF,0                                                             
         LA    R5,ADSYM_LOCTR+L'ADSYM_LOCTR-1 A(LAST BYTE OF EQU VALUE)         
         SR    R5,RF               BACK UP TO FIRST BYTE OF VALUE               
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),0(R5)       MOVE IN ENTIRE EQUATED VALUE                 
         LA    R4,1(RF,R4)         BUMP PAST END OF OUTPUT STRING               
         MVI   0(R4),C''''         DELIMIT THE SYMBOL                           
         MVC   SYMCARD+61(4),=C' EQU'                                           
*                                                                               
         MVI   SYMSORT,0           FOR SORTING OF SYMNAMES OUTPUT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         J     NEXTREC                                                          
*                                                                               
CHKHEX   DS    0H                                                               
         CLI   ADSYM_ATTR,C'X'     TYPE ATTRIBUTE IS HEX?                       
         JE    CHKHEX10            YES                                          
         MVC   P(43),=C'*** ERROR *** INVALID TYPE ATTRIBUTE:      '            
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'8'       SET BAD RETURN CODE                          
         J     NEXTREC             SKIP THIS SYMBOL                             
*                                                                               
CHKHEX10 DS    0H                                                               
         MVC   0(2,R4),=C'X'''                                                  
         LA    R4,2(R4)            BUMP TO START OF VALUE                       
*                                                                               
         ICM   R2,15,ADSYM_BYTE_LEN   LENGTH ATTRIBUTE                          
         JP    CHKHEX20                                                         
         MVC   P(43),=C'*** ERROR *** LENGTH ATTRIB. NOT POSITIVE: '            
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'8'       SET BAD RETURN CODE                          
         J     NEXTREC             SKIP THIS SYMBOL                             
*                                                                               
CHKHEX20 DS    0H                                                               
         LA    R5,ADSYM_LOCTR+L'ADSYM_LOCTR  A(JUST PAST EQUATED VALUE)         
         SR    R5,R2               BACK UP TO FIRST BYTE OF VALUE               
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,0(R5),0(R4),(R2),=C'TOG'                         
         ICM   R0,15,DMCB+16       LENGTH OF RETURNED STRING                    
         JNZ   CHKHEX30                                                         
         MVC   P(43),=C'*** ERROR *** INVALID HEX STRING:          '            
         GOTO1 =V(PRINTER)                                                      
         MVC   RETCODE,=F'8'       SET BAD RETURN CODE                          
         J     NEXTREC             SKIP THIS SYMBOL                             
*                                                                               
CHKHEX30 DS    0H                                                               
         AR    R4,R0               POINT TO END OF CONSTRUCTED STRING           
         MVI   0(R4),C''''         DELIMIT THE SYMBOL                           
         MVC   SYMCARD+61(4),=C' EQU'                                           
         MVI   SYMSORT,0           FOR SORTING OF SYMNAMES OUTPUT               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         J     NEXTREC                                                          
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE SYSADATA                                                         
*                                                                               
* Sort the symbols so that the "length symbols" (i.e., the ones that            
* are prefixed with "LEN_" sort after the positional symbols, and sort          
* all regular symbols in the sequence they appear in the original               
* assembler source. This is done merely so that the DFSORT symbol               
* listing will be in a more logical and readable sequence. It has no            
* bearing on any functionality.                                                 
*                                                                               
         OPEN  (SYMSOUT,OUTPUT)                                                 
*                                                                               
         PUT   SYMSOUT,COMMENT1                                                 
         PUT   SYMSOUT,COMMENTA                                                 
         IF (CLC,SPCLCMNT,NE,SPACES)                                            
           MVC   COMMNT6M,SPCLCMNT+20    PAN MEMBERNAME+LEVEL+DATE              
         ELSE ,                                                                 
           MVC   COMMNT6M(9),=C'*UNKNOWN*'                                      
         ENDIF ,                                                                
         PUT   SYMSOUT,COMMENT6                                                 
         PUT   SYMSOUT,COMMENTA                                                 
         PUT   SYMSOUT,COMMENT2                                                 
         PUT   SYMSOUT,COMMENT3                                                 
         PUT   SYMSOUT,COMMENT4                                                 
         PUT   SYMSOUT,COMMENT5                                                 
         PUT   SYMSOUT,COMMENTA                                                 
*                                                                               
GETSORTR DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         JZ    ENDSORTR                                                         
*                                                                               
         PUT   SYMSOUT,(R2)                                                     
         J     GETSORTR                                                         
*                                                                               
ENDSORTR DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         PUT   SYMSOUT,COMMENTA                                                 
         PUT   SYMSOUT,COMMENTX                                                 
*                                                                               
         CLOSE SYMSOUT                                                          
         DROP  R3                                                               
*                                                                               
         XBASE RC=RETCODE                                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
DTYPETAB DS    0D                  DATATYPE TRANSLATION TABLE                   
*                                                                               
* Translate the 1-character assembler type attribute (T') to its                
* equivalent 2-character DFSORT data format.                                    
*                                                                               
         DC    C'A',C'BI'          ADDRESS CONSTANT                             
         DC    C'B',C'BI'          UNSIGNED BINARY                              
         DC    C'C',C'CH'          CHARACTER                                    
         DC    C'D',C'BI'          FLOATING POINT, BUT AT DDS, BINARY!          
         DC    C'F',C'FI'          SIGNED BINARY                                
         DC    C'G',C'FI'          SIGNED BINARY                                
         DC    C'H',C'FI'          SIGNED BINARY                                
         DC    C'P',C'PD'          PACKED DECIMAL                               
         DC    C'R',C'BI'          ADDRESS CONSTANT                             
         DC    C'X',C'BI'          UNSIGNED BINARY                              
         DC    C'Z',C'ZD'          ZONED DECIMAL                                
         DC    X'FF'                                                            
*                                                                               
SYSADATA DCB   DDNAME=SYSADATA,DSORG=PS,RECFM=VB,MACRF=GM,LRECL=32756, +        
               EODAD=CLOSE,BLKSIZE=0                                            
SYMSOUT  DCB   DDNAME=SYMSOUT,DSORG=PS,RECFM=FB,MACRF=PM,LRECL=80               
*                                                                               
SORTCARD DC    C'SORT FIELDS=(XXX,XXX,BI,A),EQUALS '                            
RECCARD  DC    C'RECORD TYPE=F,LENGTH=XXX '                                     
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
CARD     DS    CL80                                                             
WORK     DS    CL17                                                             
*                                                                               
ADDRDW   DC    C'N'                'Y' = ADD 4 FOR RDW TO COLUMN NOS.           
SPCLCMNT DC    CL80' '             PANVALET "SPECIAL COMMENT" CARD              
*                                                                               
* DFSORT SYMBOL INFORMATION:                                                    
SYMBNAME_MAXLEN EQU 50             MAXIMUM DFSORT SYMBOL LENGTH                 
SYMBNAME DS    CL(SYMBNAME_MAXLEN) NAME (DFSORT ALLOWS UP TO 50 CHARS)          
COLUMN#  DS    F                   STARTING COLUMN NUMBER                       
LENGTH   DS    F                   FIELD LENGTH                                 
DATATYPE DS    CL2                 DATATYPE                                     
*                                                                               
COMMENT1 DC    CL80'*============> START OF PROGRAMMATICALLY-GENERATED +        
               SYMBOLS <==========='                                            
COMMENTA DC    CL80'*'                                                          
COMMENT6 DC    CL80'* SOURCE MEMBER:'                                           
         ORG   COMMENT6+20                                                      
COMMNT6M DS    CL38                PANVALET: MEMBERNAME+LEVEL+DATE              
         ORG   ,                                                                
COMMENT2 DC    CL80'* The following DFSORT symbols were generated **pro+        
               grammatically** by'                                              
COMMENT3 DC    CL80'* the DDBLDSYMS utility. This utility interprets DS+        
               /DC/EQU statements'                                              
COMMENT4 DC    CL80'* in an assembler DSECT member, and constructs equi+        
               valent DFSORT'                                                   
COMMENT5 DC    CL80'* symbols. See DDBLDSYMS for details.'                      
COMMENTX DC    CL80'*=============> END OF PROGRAMMATICALLY-GENERATED S+        
               YMBOLS <============'                                            
*                                                                               
         DS    0D                                                               
         DC    C'*SORTREC'                                                      
SORTREC  DS    0C                                                               
SYMCARD  DS    CL80                SYMBOL STATEMENT                             
SORTKEY  DS    0C                                                               
SYMSORT  DS    X                   TO FORCE LENGTH SYMBOLS TO THE END           
SYMSTMT# DS    XL4                 ASSEMBLER STATEMENT NUMBER                   
SORTKEYQ EQU   *-SORTKEY                                                        
SORTRECQ EQU   *-SORTREC                                                        
*                                                                               
IOAREA   DS    32760X              SYSADATA READ BUFFER                         
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDBLDSYMS 02/12/21'                                      
         END                                                                    
