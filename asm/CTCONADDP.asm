*          DATA SET CTCONADDP  AT LEVEL 002 AS OF 05/01/02                      
*PHASE CTADDPR,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
***********************************************************************         
*  TTL:      CTCONADDP: CONTROL FILE ADD PROFILE RECORDS              *         
*  PURPOSE:  ADD PROFILE RECORDS WITH NEW IDS                         *         
*                                                                     *         
***********************************************************************         
CTADDPR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CTADDPR,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         EJECT                                                                  
MAIN     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,AIO                
         MVI   DATADISP+1,28                                                    
         OPEN  (TOUT,OUTPUT)       QSAM MACRO                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
                                                                                
                                                                                
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
         L     R6,AIO              POINT AT REC READ                            
         B     M20                                                              
                                                                                
M10      GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,AIO               
         CLI   DMCB+8,X'80'        NO MORE RECORDS?                             
         BE    M50                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE, BAD READ                                
                                                                                
M20      CLI   0(R6),CTIKTYPQ      ID RECORD?                                   
         BH    M30                                                              
         BNE   M40                                                              
         BAS   RE,ADDID                                                         
         BAS   RE,SPUT                                                          
         B     M10                                                              
                                                                                
M30      CLI   TABLEOK,C'Y'        TABLE OF ID CODES COMPLETE?                  
         BE    M40                                                              
         BAS   RE,CHKTAB           CHECK IF TABLE IS COMPLETE                   
                                                                                
M40      BAS   RE,SPUT             SEND ORIGINAL RECORD TO SORT                 
         CLI   0(R6),CTPKTYPQ      PROFILE RECORD?                              
         BNE   M10                                                              
         BAS   RE,ADDPR                                                         
         B     M10                                                              
                                                                                
M50      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    MX                                                               
         CLC   KEYSAVE,4(R6)                                                    
         BNE   M60                                                              
         BAS   RE,DUPKEY                                                        
         B     M50                                                              
                                                                                
M60      PUT   TOUT,(R6)                                                        
         MVC   KEYSAVE,4(R6)                                                    
         B     M50                                                              
                                                                                
MX       GOTO1 =V(SORTER),DMCB,=C'END'                                          
         MVC   P(5),=C'COUNT'                                                   
         EDIT  (2,COUNT),(4,P+15)                                               
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
* ADDID: PUT ID CODE IN TABLE                                         *         
***********************************************************************         
         SPACE                                                                  
ADDID    NTR1                                                                   
         L     R7,AIO                                                           
         USING CTIREC,R7                                                        
         LA    R2,TABLE                                                         
                                                                                
ADDID10  CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    ADDIDX                                                           
                                                                                
         CLC   CTIKID,0(R2)        SAME ID?                                     
         BE    *+12                                                             
         LA    R2,12(R2)           NEXT ID                                      
         B     ADDID10                                                          
                                                                                
         MVI   ELCODE,X'02'        GET 02 ELEM (DESCRIPTION - ID NUM)           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   10(2,R2),2(R6)      MOVE ID CODE FROM ELEM INTO TABLE            
                                                                                
         DROP  R7                                                               
ADDIDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHKTAB: CHECK THAT ALL ID'S HAVE CODES                              *         
***********************************************************************         
         SPACE                                                                  
CHKTAB   NTR1                                                                   
         LA    R2,TABLE                                                         
                                                                                
CHKTAB10 CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    CHKTABX                                                          
                                                                                
         OC    10(2,R2),10(R2)     ZEROS?                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,(R2),P,12                                        
         GOTO1 =V(PRINTER)                                                      
         LA    R2,12(R2)           NEXT ID                                      
         B     CHKTAB10                                                         
                                                                                
CHKTABX  MVI   TABLEOK,C'Y'        TABLE IS OK                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADDPR: ADD A PROFILE RECORD WITH NEW ID IF OLD ID IS IN TABLE       *         
***********************************************************************         
         SPACE                                                                  
ADDPR    NTR1                                                                   
         L     R7,AIO                                                           
         USING CTPREC,R7                                                        
         LA    R2,TABLE                                                         
                                                                                
ADDPR10  CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    ADDPRX                                                           
                                                                                
         CLC   CTPKORIG,10(R2)     SAME ID?                                     
         BE    *+12                                                             
         LA    R2,24(R2)           NEXT ID                                      
         B     ADDPR10                                                          
                                                                                
         MVC   CTPKORIG,22(R2)     MOVE NEW ID INTO RECORD                      
         BAS   RE,SPUT                                                          
                                                                                
         LH    R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STH   R1,COUNT                                                         
                                                                                
         GOTO1 =V(HEXOUT),DMCB,(R7),P,25                                        
         GOTO1 =V(PRINTER)                                                      
         DROP  R7                                                               
ADDPRX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SPUT: SORT PUT, SET LEN, 2 BYTES OF NULLS, AND VERIFY VALID LENGTH  *         
***********************************************************************         
         SPACE                                                                  
SPUT     NTR1                                                                   
         L     R7,AIO                                                           
         L     R2,ARECLEN                                                       
                                                                                
* IS THE RECORD LENGTH ALWAYS AT 25                                             
SP10     MVC   0(2,R2),25(R7)      GET RECORD LEN FOR PUT                       
         LA    R1,4                ADD 4 TO COVER 2 BYTES LEN + ...             
         AH    R1,0(R2)            2 BYTES OF NULLS                             
         STH   R1,0(R2)                                                         
         CH    R1,=H'32'           25B KEY + 4B ABOVE + 3B(LEN+STAT)            
         BNL   *+6                                                              
         DC    H'0'                DIE, BAD LENGTH                              
         GOTO1 =V(SORTER),DMCB,=C'PUT',ARECLEN                                  
                                                                                
SPX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DUPKEY: PRINT DUPLICATE KEY ERROR                                   *         
***********************************************************************         
         SPACE                                                                  
DUPKEY   NTR1                                                                   
         MVC   P(22),=C'ERROR, DUPLICATE KEY: '                                 
         LA    R6,4(R6)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R6),P+25,25,=C'TOG'                             
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(CTADDPR,65000)                                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2048'                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
ARECLEN  DC    A(RECLEN)                                                        
AIO      DC    A(IO)                                                            
*                                                                               
COUNT    DS    H                                                                
DUB      DS    D                                                                
WORK     DS    CL17                                                             
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
*                                                                               
TABLEOK  DC    C'N'                                                             
*                OLDID           CODE    NEWID          CODE                    
TABLE    DC    C'BSNYA     ',XL2'0000',C'ZBSNYA    ',XL2'0000'                  
         DC    C'BSNYAN    ',XL2'0000',C'ZBSNYAN   ',XL2'0000'                  
         DC    C'BSNYAO    ',XL2'0000',C'ZBSNYAO   ',XL2'0000'                  
         DC    C'BSNYRE    ',XL2'0000',C'ZBSNYRE   ',XL2'0000'                  
         DC    C'BSNYREN   ',XL2'0000',C'ZBSNYREN  ',XL2'0000'                  
         DC    C'BSNYREO   ',XL2'0000',C'ZBSNYREO  ',XL2'0000'                  
         DC    C'BSHO      ',XL2'0000',C'ZBSHO     ',XL2'0000'                  
         DC    C'BSHORE    ',XL2'0000',C'ZBSHORE   ',XL2'0000'                  
         DC    C'DFCF      ',XL2'0000',C'ZDFCF     ',XL2'0000'                  
         DC    C'DFCH      ',XL2'0000',C'ZDFCH     ',XL2'0000'                  
         DC    C'DFDN      ',XL2'0000',C'ZDFDN     ',XL2'0000'                  
         DC    C'DFLA      ',XL2'0000',C'ZDFLA     ',XL2'0000'                  
         DC    C'DFNYCME   ',XL2'0000',C'ZDFNYCME  ',XL2'0000'                  
         DC    C'DFNYRE    ',XL2'0000',C'ZDFNYRE   ',XL2'0000'                  
         DC    C'DFNY      ',XL2'0000',C'ZDFNY     ',XL2'0000'                  
         DC    C'LFCH      ',XL2'0000',C'ZLFCH     ',XL2'0000'                  
         DC    C'LFDN      ',XL2'0000',C'ZLFDN     ',XL2'0000'                  
         DC    C'LFLA      ',XL2'0000',C'ZLFLA     ',XL2'0000'                  
         DC    C'LFNY      ',XL2'0000',C'ZLFNY     ',XL2'0000'                  
         DC    C'SFCH      ',XL2'0000',C'ZSFCH     ',XL2'0000'                  
         DC    C'SFLA      ',XL2'0000',C'ZSFLA     ',XL2'0000'                  
         DC    C'SFNY      ',XL2'0000',C'ZSFNY     ',XL2'0000'                  
         DC    C'CECH      ',XL2'0000',C'ZCECH     ',XL2'0000'                  
         DC    C'CECOR     ',XL2'0000',C'ZCECOR    ',XL2'0000'                  
         DC    C'CEMNRE    ',XL2'0000',C'ZCEMNRE   ',XL2'0000'                  
         DC    C'CEMNNS    ',XL2'0000',C'ZCEMNNS   ',XL2'0000'                  
         DC    C'CEMNDN    ',XL2'0000',C'ZCEMNDN   ',XL2'0000'                  
         DC    C'CEMNDF    ',XL2'0000',C'ZCEMNDF   ',XL2'0000'                  
         DC    C'CEMN      ',XL2'0000',C'ZCEMN     ',XL2'0000'                  
         DC    X'FF'                                                            
*                                                                               
DMCB     DS    6F                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTCONADDP 05/01/02'                                      
         END                                                                    
