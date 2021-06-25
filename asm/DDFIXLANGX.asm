*          DATA SET DDFIXLANGX AT LEVEL 001 AS OF 08/31/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE FIXLNGXA                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*                                                                               
***********************************************************************         
*                                                                               
*                    IDF: ASMLANGX BUG WORKAROUND                               
*                                                                               
* This program is a workaround for a bug in IBM's ASMLANGX utility.             
* ASMLANGX generates the source code extract files which are used by            
* IDF. This program is intended to correct a specific error which we            
* regularly see in ASMLANGX-generated files.                                    
*                                                                               
* Here's the background: we are using the HLASM "program type"                  
* attribute to classify some of our data fields (e.g., compressed               
* dates). There is an unfortunate bug in ASMLANGX, viz.: the presence           
* of a *program type* on a DC or DS statement causes ASMLANGX to ignore         
* the *length* attribute of the statement's symbol. This often results          
* in an incorrect display within an IDF Variable Window.                        
*                                                                               
* For example, suppose a field is legitimately defined this way:                
*                                                                               
*    BUYDATE  DS    XP(DT02)L2         length attribute is *two*                
*                                                                               
* If field BUYDATE is displayed in an IDF Variable window, IDF will             
* ignore its length attribute, and only *one* byte will be displayed            
* in the Variable window.                                                       
*                                                                               
* Why does this happen? As per IBM: ASMLANGX *parses* each DS/DC                
* statement to derive each symbol's length attribute (rather than               
* extracting it from the ADATA file, which IBM acknowledges would have          
* been the correct way to do it). Because ASMLANGX's parse logic was            
* never updated by IBM to recognize the possible presence of a program          
* type, ASMLANGX never "sees" the symbol's length attribute. I.e., it           
* incorrectly deduces that the symbol has no *explicit* length                  
* attribute, and it therefore assigns the *implicit* length attribute           
* for the symbol's *type*, which is *one* for fields of type C and X.           
*                                                                               
* IBM says that this bug is extremely unlikely to be fixed, due to both         
* a lack of user demand, and a lack of developer resources.                     
*                                                                               
* Therefore, we are left to resolve this issue on our own. Our solution         
* is to analyze and (if necessary) *modify* the ASMLANGX-generated              
* dataset before it is written to disk. We do this by reading both the          
* ADATA file and the ASMLANGX file. If the length attributes for a              
* given symbol are not identical in both files, then the length                 
* attribute in the ASMLANGX file is overwritten with the length                 
* attribute found in the ADATA file.                                            
*                                                                               
* The only problem with this approach is that the ASMLANGX file format          
* is not published by IBM. Both the *identification* of the ASMLANGX            
* symbol records, as well as their *structure*, have been deduced               
* entirely by empirical observation. It therefore seems prudent to make         
* the absolute minimum number of modifications to the ASMLANGX file as          
* possible. In particular: *we only modify the length attribute of a            
* field which is defined with a program type*, because that's the only          
* case where we've ever run into trouble within IDF.                            
*                                                                               
* We do know from IBM's documentation that the ASMLANGX file has the            
* following attributes: RECFM=VB,LRECL=1562,BLKSIZE=27998.                      
* Beyond that, the following assumptions have been made regarding the           
* structure of the ASMLANGX file format (all displacements are based            
* off the very beginning of the record, i.e., at the RDW location):             
*                                                                               
*   1. Offset 0(4): RDW.                                                        
*   2. Offset 4(1): record type.                                                
*   3. Record type X'24' denotes a "symbol" record. Within a symbol             
*      record:                                                                  
*     a. The *low-order* byte of the the length attribute is at offset          
*        X'14'. It is unknown how many bytes are actually reserved for          
*        the length attribute. For that reason, this program only               
*        relies on the low-order *two* bytes of the length attribute            
*        (even though the ADATA file reserves *four* bytes for this             
*        value). This allows for a maximum length attribute of 65535,           
*        which is more than adequate for our purposes.                          
*     b. The length of the symbol is at offset X'28'. This is assumed           
*        to be a one-byte value, because the maximum symbol length              
*        supported by HLASM is 63.                                              
*     c. The symbol itself is of variable-length, and begins at offset          
*        X'29'.                                                                 
*                                                                               
* There are other assumptions we can make regarding "duplication                
* factor" records, but this program does not rely on those assumptions.         
* They are documented below, just in case we ever need to rely on them.         
*                                                                               
* If this program ever stops working, or if it works in what appears            
* to be an "incorrect" manner (i.e., if IDF behaves in an unexpected            
* manner), it may be because one or more of the assumptions listed              
* above are incorrect. And of course, there is nothing to stop IBM from         
* modifying the structure of the ASMLANGX dataset in the future, which          
* has the potential to break this program's algorithm. However, given           
* IBM's reluctance to fix the ASMLANGX bug in the first place, we can           
* probably safely assume that the current ASMLANGX data format will not         
* be changed in the foreseeable future.                                         
*                                                                               
***********************************************************************         
         EJECT                                                                  
         ASMADATA PRINT=GEN,SYM=YES                                             
         EJECT                                                                  
FIXLANGX CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,FIXLANGX,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         L     R2,=A(MAXSYMS)                                                   
         MHI   R2,ADATAINFLNQ                                                   
         LA    R2,16(,R2)          FOR EYE-CATCHER(16)                          
         STORAGE OBTAIN,LENGTH=(R2),LOC=ANY,BNDRY=PAGE                          
         SAM31 ,                                                                
         MVC   0(16,R1),=C'**ADATA SYMBOLS*'  R1 = A(OBTAINED STORAGE)          
         LA    R1,16(,R1)          BUMP PAST EYE-CATCHER                        
         ST    R1,ASYMTAB          SAVE A(SYMBOL TABLE)                         
         SAM24 ,                                                                
*                                                                               
         OPEN  ADATA                                                            
*                                                                               
* BUILD A BINSRCH TABLE OF SYMBOLS FROM THE ADATA FILE, WITH THEIR              
* RELEVANT ATTRIBUTES.                                                          
*                                                                               
GETADATA DS    0H                                                               
         GET   ADATA,IOAREA        READ ADATA RECORD                            
*                                                                               
         USING ASMADATA,R3                                                      
         LA    R3,IOAREA                                                        
*                                                                               
         CLI   ADATA_VERSION,ADATA_VERASM  MUST BE ASSEMBLER SOURCE             
         JNE   GETADATA                                                         
         CLI   ADATA_LEVEL,ADATA_L3        MUST BE ARCHITECTURE LEVEL 3         
         JNE   GETADATA                                                         
         CLC   ADATA_RECTYPE,=AL2(ADATA_RECSYM)  ONLY KEEP SYMBOL RECS.         
         JNE   GETADATA                                                         
         CLI   ADSYM_TYPE,ADSYM_TYPE_ORDINARY    (E.G., DS, DC)                 
         JNE   GETADATA                  NOT DS/DC: SKIP IT                     
         OC    ADSYM_PROGRAM_TYPE,ADSYM_PROGRAM_TYPE ONLY MODIFY SYMBOL         
         JZ    GETADATA                     ...IF IT HAS A PROGRAM TYPE         
*                                                                               
         MVC   ADATA_SYMBOL,SPACES BUILD A BLANK-PADDED SYMBOL FIELD            
         ICM   R1,15,ADSYM_NAME_LEN                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ADATA_SYMBOL(0),ADSYM_NAME                                       
         MVC   ADATA_LENATTR,ADSYM_BYTE_LEN   SAVE LENGTH ATTRIBUTE             
         MVC   ADATA_DUPFACTR,ADSYM_DUP       SAVE DUPLICATION FACTOR           
         SAM31 ,                                                                
         GOTO1 =V(BINSRCH),DMCB,ADATAINF,ASYMTAB,NUMRECS,              +        
               (1,ADATAINFLNQ),L'ADATA_SYMBOL,A(MAXSYMS)                        
         SAM24 ,                                                                
         OC    DMCB(4),DMCB                                                     
         JZ    *+2                 BINSRCH TABLE IS FULL ?!?                    
         MVC   NUMRECS,DMCB+8      UPDATE TABLE ENTRY COUNT                     
         B     GETADATA                                                         
*                                                                               
CLSADATA DS    0H                                                               
         CLOSE ADATA                                                            
*                                                                               
         OPEN  LANGXIN             READ FROM ORIGINAL ASMLANGX FILE             
         OPEN  (LANGXOUT,OUTPUT)   WRITE TO MODIFIED ASMLANGX FILE              
*                                                                               
* LOOK UP EACH ASMLANGX SYMBOL IN THE BINSRCH TABLE. IF IT'S THERE,             
* AND IF THE LENGTH ATTRIBUTES DON'T MATCH, WRITE AN UPDATED ASMLANGX           
* RECORD.                                                                       
*                                                                               
GETLANGX DS    0H                                                               
         GET   LANGXIN,IOAREA      GET ASMLANGX RECORD                          
*                                                                               
         USING LANGX_RECORD,R5                                                  
         LA    R5,IOAREA                                                        
*                                                                               
         CLI   LANGX_RECORD_TYPE,LANGX_RECORD_TYPE_SYMBOL                       
         BNE   PUTLANGX            WE ONLY CARE ABOUT X'24' RECORDS             
*                                                                               
         MVC   ADATA_SYMBOL,SPACES BUILD A BLANK-PADDED SYMBOL FIELD            
         SR    R1,R1                                                            
         ICM   R1,1,LANGX_SYMBOL_LENGTH                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ADATA_SYMBOL(0),LANGX_SYMBOL                                     
         SAM31 ,                                                                
         GOTO1 =V(BINSRCH),DMCB,ADATAINF,ASYMTAB,NUMRECS,              +        
               ADATAINFLNQ,L'ADATA_SYMBOL,A(MAXSYMS)                            
         TM    0(R1),X'80'   NOT FOUND?                                         
         BO    PUTLANGX      CORRECT: WE DON'T CARE ABOUT THIS SYMBOL           
         ICM   R3,15,0(R1)   A(FOUND SYMBOL)                                    
         JZ    *+2           IF RECORD FOUND, WE SHOULD HAVE AN ADDRESS         
*                                                                               
         CLC   LANGX_LENATTR,ADATA_LENATTR+2                                    
         BE    PUTLANGX      LENGTH ATTRIBUTE IS UNCHANGED                      
*                                                                               
         MVC   P(52),=C'ASMLANGX LEN=XXXX. ADATA LEN=XXXX, DUP=XXXX, SY+        
               MBOL='                                                           
         EDIT  LANGX_LENATTR,(4,P+13),ZERO=NOBLANK                              
         EDIT  (B4,ADATA_LENATTR),(4,P+29),ZERO=NOBLANK                         
         EDIT  (B4,ADATA_DUPFACTR),(4,P+39),ZERO=NOBLANK                        
         MVC   P+52(L'ADATA_SYMBOL),ADATA_SYMBOL                                
*                                  UPDATE THE LENGTH ATTRIBUTE                  
         MVC   LANGX_LENATTR,ADATA_LENATTR+2                                    
         SAM24 ,                                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*&&DO                                                                           
* There are some other empirical observations we can make regarding             
* the ASMLANGX file structure, but we don't rely on them, because we            
* don't need to. Viz.: there is a X'80' bit set in the symbol record            
* when (and only when) there is a X'12' "duplication factor" record             
* immediately following the symbol record. The X'12' record contains            
* any non-zero duplication factor for the symbol.                               
         PUT   LANGXOUT,IOAREA PUT THE MODIFIED SYMBOL RECORD                   
         TM    LANGX_SYMBOL_FLAGS,LANGX_DUP_FACTOR_RECORD_PRESENT               
         BZ    GETLANGX        GO BACK FOR NEXT RECORD                          
*                                                                               
         GET   LANGXIN,IOAREA  GET THE X'12' DUPLICATION FACTOR RECORD          
         CLI   LANGX_RECORD_TYPE,LANGX_RECORD_TYPE_DUP_FACTOR                   
         JNE   *+2             ASSUMPTION WAS INCORRECT!                        
         MVC   P(19),=C'ASMLANGX DUP=XXXXXX'                                    
         EDIT  LANGX_DUPLICATION_FACTOR,(6,P+13),ZERO=NOBLANK                   
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
PUTLANGX DS    0H                                                               
         SAM24 ,                                                                
*                                                                               
         PUT   LANGXOUT,IOAREA     PUT THE MODIFIED SYMBOL RECORD               
         J     GETLANGX                                                         
*                                                                               
CLSLNGX  DS    0H                                                               
         CLOSE LANGXIN                                                          
         CLOSE LANGXOUT                                                         
*                                                                               
         DROP  R3,R5                                                            
*                                                                               
         XBASE RC=RETCODE                                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
ADATA    DCB   DDNAME=ADATA,DSORG=PS,RECFM=VB,MACRF=GM,LRECL=32756,    +        
               EODAD=CLSADATA,BLKSIZE=32760                                     
LANGXIN  DCB   DDNAME=LANGXIN,DSORG=PS,RECFM=VB,MACRF=GM,LRECL=1562,   +        
               EODAD=CLSLNGX,BLKSIZE=0                                          
LANGXOUT DCB   DDNAME=LANGXOUT,DSORG=PS,RECFM=VB,MACRF=PM,LRECL=1562            
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
NUMRECS  DC    A(0)                NUMBER OF ENTRIES IN SYMBOL TABLE            
MAXSYMS  EQU   65535               MAXIMUM NUMBER OF SUPPORTED SYMBOLS          
ASYMTAB  DS    A                   A(SYMBOL TABLE)                              
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
WORK     DS    CL17                                                             
*                                                                               
ADATAINF       DS 0C               SYMBOL INFO FROM ADATA FILE                  
ADATA_SYMBOL   DS CL63             SYMBOL                                       
ADATA_LENATTR  DS FL4              LENGTH ATTRIBUTE                             
ADATA_DUPFACTR DS FL4              DUPLICATION FACTOR                           
ADATAINFLNQ    EQU *-ADATAINF                                                   
*                                                                               
IOAREA   DS    32760X              READ BUFFER                                  
*                                                                               
* SEE FLOWERBOX FOR SPECIFICS REGARDING THESE RECORD LAYOUTS, AND HOW           
* THEY WERE DERIVED !!!                                                         
*                                                                               
LANGX_RECORD DSECT ,                                                            
LANGX_RDW                    DS  XL4      RDW                                   
LANGX_RECORD_TYPE            DS  X        RECORD TYPE                           
LANGX_RECORD_TYPE_SYMBOL     EQU X'24'    SYMBOL RECORD                         
LANGX_RECORD_TYPE_DUP_FACTOR EQU X'12'    DUPLICATION FACTOR RECORD             
LANGX_RECORD_DATA            DS  0C       (RECORD-TYPE DEPENDENT)               
*                                                                               
* SYMBOL RECORD:                                                                
                             DS  XL12     UNKNOWN                               
*   (WE DON'T KNOW THE LENGTH OF THE LENGTH ATTRIBUTE FIELD! IT'S               
*    PROBABLY 4 BYTES, BUT WE ONLY RELY ON IT BEING 2 BYTES.)                   
                             DS  XL2      (MIGHT BE PART OF NEXT FIELD)         
LANGX_LENATTR                DS  FL2      LENGTH ATTRIBUTE                      
                             DS  XL14     UNKNOWN                               
LANGX_SYMBOL_FLAGS           DS  X        VARIOUS FLAGS? (UNKNOWN)              
LANGX_DUP_FACTOR_RECORD_PRESENT EQU X'80' (EDUCATED GUESS)                      
                             DS  XL4      UNKNOWN                               
LANGX_SYMBOL_LENGTH          DS  XL1      SYMBOL LENGTH                         
LANGX_SYMBOL                 DS  0C       SYMBOL (VARIABLE LENGTH)              
                             ORG LANGX_RECORD_DATA                              
*                                                                               
* DUPLICATION FACTOR RECORD:                                                    
                             DS  XL7      UNKNOWN                               
LANGX_DUPLICATION_FACTOR     DS  XL4      DUPLICATION FACTOR                    
                             ORG ,                                              
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDFIXLANGX08/31/20'                                      
         END                                                                    
