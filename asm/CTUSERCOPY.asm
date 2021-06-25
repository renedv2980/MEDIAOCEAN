*          DATA SET CTUSERCOPY AT LEVEL 011 AS OF 08/10/00                      
*PHASE USERCOPA USERCOPY                                                        
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'CTUSERCOPY - COPY USER PROFILE RECS'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*  PURPOSE:  READ ALL USER PROFILE RECS FROM CTFILE.  IF AGY IS 'PU', *         
*            CREATE A COPY RECORD FOR AGY 'QA' FOR SYSTEM SPOT        *         
*                                                                     *         
*            ALSO COPY ALL FAX RECORDS FOR 'PU' TO 'QA'               *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
USERCOPY CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**COPY**,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
         EJECT                                                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,IO                 
         OPEN  (TOUT,OUTPUT)       QSAM MACRO                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
* SEQUENTIALLY READ ALL RECS IN CTFILE AND WRITE TO FILE,                       
* COPYING PROFILES WHEN NECESSARY                                               
         XC    KEY,KEY                                                          
         LA    R7,IO                                                            
         GOTO1 =V(DATAMGR),DMCB,(X'80',=C'DMRDHI'),=C'CTFILE',KEY,IO            
*                                                                               
MX10     DS    0H                                                               
         L     RF,TOTRECS          UPDATE TOTAL RECORD COUNTER                  
         A     RF,=F'1'                                                         
         ST    RF,TOTRECS                                                       
*                                                                               
         USING CTUREC,R7                                                        
         CLI   CTUKTYP,CTUKTYPQ    PROFILE REC?                                 
         BNE   MX25                 NO - CHECK FOR FAX REC                      
         L     RF,TOTUSER          UPDATE U-REC COUNTER                         
         A     RF,=F'1'                                                         
         ST    RF,TOTUSER                                                       
*                                                                               
         CLI   CTUKSYS,C'S'        SPOT?                                        
         BNE   MX30                 NO                                          
         CLC   CTUKAGY,=C'PU'      RIGHT AGY?                                   
         BE    MX20                 YES                                         
*                                                                               
         CLC   CTUKAGY,=C'QA'      MAKE SURE THERE ARE NONE...                  
         BNE   MX30                 NO                                          
         LA    R2,P                ELSE SHOW THEM!                              
         MVC   0(5,R2),=C'*****'                                                
         LA    R2,6(R2)                                                         
         MVC   0(L'CTUKSYS,R2),CTUKSYS                                          
         LA    R2,L'CTUKSYS+2(R2)                                               
         MVC   0(L'CTUKPROG,R2),CTUKPROG                                        
         LA    R2,L'CTUKPROG+2(R2)                                              
         MVC   0(L'CTUKAGY,R2),CTUKAGY                                          
         LA    R2,L'CTUKAGY+2(R2)                                               
         MVC   0(L'CTUKMED,R2),CTUKMED                                          
         LA    R2,L'CTUKMED+2(R2)                                               
         MVC   0(L'CTUKCLT,R2),CTUKCLT                                          
         LA    R2,L'CTUKCLT+10(R2)                                              
         GOTO1 =V(PRINTER)                                                      
         B     MX30                                                             
*                                                                               
MX20     DS    0H                                                               
*&&DO                                                                           
         CLI   CTUKPROG+1,C'$'     NOT OFFICE LISTS                             
         BNE   *+12                                                             
         CLI   CTUKPROG,0                                                       
         BE    MX30                                                             
*                                                                               
         OC    CTUKCLT,CTUKCLT     CLT SPECIFIC?                                
         BNZ   MX30                 YES - LEAVE IT ALONE                        
*&&                                                                             
*                                                                               
* PRINT KEY                                                                     
         LA    R2,P                                                             
         MVC   0(L'CTUKSYS,R2),CTUKSYS                                          
         LA    R2,L'CTUKSYS+2(R2)                                               
         MVC   0(L'CTUKPROG,R2),CTUKPROG                                        
         LA    R2,L'CTUKPROG+2(R2)                                              
         MVC   0(L'CTUKAGY,R2),CTUKAGY                                          
         LA    R2,L'CTUKAGY+2(R2)                                               
         MVC   0(L'CTUKMED,R2),CTUKMED                                          
         LA    R2,L'CTUKMED+2(R2)                                               
         MVC   0(L'CTUKCLT,R2),CTUKCLT                                          
         LA    R2,L'CTUKCLT+10(R2)                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* PUT OUT REC, CHANGE AGY TO 'QA' PUT NEW REC                                   
         BAS   RE,SPUT                                                          
         MVC   CTUKAGY,=C'QA'                                                   
*                                                                               
         L     RF,QAADD            UPDATE ADDED COUNTER                         
         A     RF,=F'1'                                                         
         ST    RF,QAADD                                                         
         B     MX30                                                             
         DROP  R7                                                               
*                                                                               
MX25     DS    0H                                                               
         USING CTFXREC,R7                                                       
         CLI   CTFXKTYP,CTFXEQU    FAX RECORD?                                  
         BNE   MX30                 NO - JUST PUT IT OUT                        
         L     RF,TOTFAX           UPDATE U-REC COUNTER                         
         A     RF,=F'1'                                                         
         ST    RF,TOTFAX                                                        
*                                                                               
         CLC   CTFXAGY,=C'PU'      RIGHT AGY?                                   
         BE    MX26                 YES                                         
*                                                                               
         CLC   CTFXAGY,=C'QA'      MAKE SURE THERE ARE NONE...                  
         BNE   MX30                 NO - JUST PUT IT                            
         LA    R2,P                ELSE SHOW THEM!                              
         MVC   0(5,R2),=C'*****'                                                
         LA    R2,6(R2)                                                         
         MVC   0(L'CTFXAGY,R2),CTFXAGY                                          
         LA    R2,L'CTFXAGY+2(R2)                                               
         MVC   0(L'CTFXCODE,R2),CTFXCODE                                        
         LA    R2,L'CTFXCODE+2(R2)                                              
         MVC   0(L'CTFXSUBC,R2),CTFXSUBC                                        
         GOTO1 =V(PRINTER)                                                      
         B     MX30                                                             
*                                                                               
MX26     DS    0H                                                               
         LA    R2,P                PRINT KEY OF COPIED RECS                     
         MVC   0(L'CTFXAGY,R2),CTFXAGY                                          
         LA    R2,L'CTFXAGY+2(R2)                                               
         MVC   0(L'CTFXCODE,R2),CTFXCODE                                        
         LA    R2,L'CTFXCODE+2(R2)                                              
         MVC   0(L'CTFXSUBC,R2),CTFXSUBC                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* PUT OUT REC, CHANGE AGY TO 'QA' PUT NEW REC                                   
         BAS   RE,SPUT                                                          
         MVC   CTFXAGY,=C'QA'                                                   
*                                                                               
         L     RF,FAXADD          UPDATE ADDED COUNTER                          
         A     RF,=F'1'                                                         
         ST    RF,FAXADD                                                        
         DROP  R7                                                               
*                                                                               
MX30     DS    0H                                                               
         BAS   RE,SPUT                                                          
*                                                                               
* GET NEXT REC                                                                  
         GOTO1 =V(DATAMGR),DMCB,(X'80',=C'DMRSEQ'),=C'CTFILE',KEY,IO            
         TM    DMCB+8,X'80'        EOF?                                         
         BZ    MX10                 NO                                          
         EJECT                                                                  
*                                                                               
* GET ALL RECS FROM SORTER AND PUT TO OUTPUT TAPE, NO DUP CHECKING              
MX50     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    MX60                                                             
         PUT   TOUT,(R6)                                                        
         B     MX50                                                             
*                                                                               
MX60     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE TOUT                                                             
         BAS   RE,PRNCTRS                                                       
MXB      XBASE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  SPUT: PUT REC TO SORTER WITH 2 BYTE LEN & 2 BYTES NULLS            *         
***********************************************************************         
         SPACE                                                                  
SPUT     NTR1                                                                   
         LA    R7,IO                                                            
         USING CTUREC,R7                                                        
         XR    R1,R1                                                            
         ICM   R1,3,CTULEN         GET RECLEN FOR PUT                           
         AHI   R1,4                + 4 FOR 2 BYTES LEN & 2 BYTES NULLS          
         STH   R1,RECLEN                                                        
         CHI   R1,32               25B KEY + 4B ABOVE + 3B(LEN+STAT)            
         BNL   *+6                                                              
         DC    H'0'                DIE - BAD REC LEN                            
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECLEN                                   
SPX      B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*  PRNCTRS: PRINT OUTPUT COUNTERS                                     *         
***********************************************************************         
         SPACE                                                                  
PRNCTRS  NTR1                                                                   
         MVC   LINE,=PL2'75'       FORCE NEW PAGE                               
         LA    R2,P                                                             
         MVC   P(14),=C'TOTAL RECORDS:'                                         
         EDIT  TOTRECS,(10,40(R2))                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'TOTAL USER PROFILE RECORDS:'                            
         EDIT  TOTUSER,(10,40(R2))                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(18),=C'TOTAL FAX RECORDS:'                                     
         EDIT  TOTFAX,(10,40(R2))                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(34),=C'USER PROFILE RECORDS ADDED FOR QA:'                     
         EDIT  QAADD,(10,40(R2))                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(25),=C'FAX RECORDS ADDED FOR QA:'                              
         EDIT  FAXADD,(10,40(R2))                                               
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
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
FLIST    DS    0H                                                               
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         DC    C'**KEY***'                                                      
KEY      DS    XL25                                                             
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL255                                                            
*                                                                               
TOTRECS  DC    F'0'                                                             
TOTUSER  DC    F'0'                                                             
TOTFAX   DC    F'0'                                                             
QAADD    DC    F'0'                                                             
FAXADD   DC    F'0'                                                             
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                                                                
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(USERCOPY,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011CTUSERCOPY08/10/00'                                      
         END                                                                    
