*          DATA SET ACREPI902  AT LEVEL 009 AS OF 09/25/00                      
*PHASE ACI902A                                                                  
         TITLE 'ACI902-BACKER-PROD VENDOR TAPE'                                 
ACI902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACI9**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACI9D,RC                                                         
*                                                                               
********************************************************************            
* ACREPI902 WRITES A TAPE WITH VENDOR CODES, NAMES AND ADDRESSES.  *            
* AN OPTIONAL REPORT MAY BE GENERATED AT WHICH LISTS THE TAPE      *            
* RECORDS IN ACCOUNT CODE ORDER.                                   *            
*                                                                  *            
* TAPE LAYOUT:   POSITION        DATA                              *            
*                 1-12           ACCOUNT CODE                      *            
*                13-49           ACCOUNT NAME                      *            
*                50-76           ADDRESS LINE 1                    *            
*                77-103          ADDRESS LINE 2                    *            
*               104-130          ADDRESS LINE 3                    *            
*               131-157          ADDRESS LINE 4                    *            
********************************************************************            
         EJECT                                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   TP20                                                             
*                                                                               
         ZAP   TOTRECS,=P'0'                                                    
*                                                                               
         MVC   DUB,SPACES                                                       
         LA    R6,TAPEOUT                                                       
         MVC   DSNAME,=CL20'ACCTAPE.AC0I9XX'                                    
         L     R1,ADCMPEL                                                       
         USING ACCOMPD,R1                                                       
         MVC   DSNAME+13(2),ACMPALFA                                            
         MVC   DUB(7),=C'TAPEOUT'                                               
         GOTO1 DYNALLOC,DMCB,DUB,DSNAME       GENERATE DD STATEMENTS            
         OPEN  ((R6),(OUTPUT))                                                  
         B     TPEXT                                                            
         EJECT                                                                  
TP20     CLI   MODE,PROCACC       ACCOUNT REC?                                  
         BNE   TP30                                                             
*                                                                               
         AP    TOTRECS,=P'1'       UPDATE TOTAL RECORD COUNT                    
         MVI   TAPEREC,X'40'                                                    
         LA    R1,TAPEREC                                                       
         MVC   1(159,R1),0(R1)     INIT TAPEREC TO SPACES                       
*                                                                               
         LA    R5,P                                                             
         L     R4,ADACC            A(REC)                                       
         USING ACKEYD,R4                                                        
         MVC   TAPECODE,ACKEYACC+3 ACCOUNT CODE TO TAPE FIELD                   
         MVC   1(12,R5),ACKEYACC+3 ACCOUNT CODE TO PRINT LINE                   
         DROP  R4                                                               
*                                                                               
* ACCOUNT NAMES                                                                 
         L     R4,ADACCNAM         A(ACCOUNT NAME ELEMENT)                      
         USING ACNAMED,R4                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TAPENAME(0),ACNMNAME TO TAPE REC                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   16(0,R5),ACNMNAME    TO PRINT LINE                               
         DROP  R4                                                               
*                                                                               
* ADDRESS FIELDS                                                                
         L     R4,ADACCADD                                                      
         USING ACADDD,R4                                                        
         CLI   0(R4),X'22'                                                      
         BNE   TPEXT               NO ADDRESS                                   
         LA    R7,TAPEADD1         A(1ST ADDR TAPE LOC)                         
         LA    R6,ACADADD          A(1ST ADDR LINE)                             
         ZIC   R2,ACADLNES         NUMBER OF ADDR LINES                         
TP25     MVC   0(26,R7),0(R6)      ADDR TO TAPE                                 
         MVC   55(26,R5),0(R6)     ADDR TO PRINT LINE                           
         LA    R5,132(R5)          =A(NEXT PRINT LINE)                          
         LA    R6,26(R6)           NEXT ADDR LINE                               
         LA    R7,26(R7)           NEXT ADDR LINE TAPE LOC                      
         BCT   R2,TP25                                                          
         GOTO1 ACREPORT                                                         
         MVI   SPACING,2                                                        
*                                                                               
         LA    R7,TAPEREC                                                       
         PUT   TAPEOUT,(R7)        REC TO TAPE                                  
         B     TPEXT                                                            
         EJECT                                                                  
*                                                                               
TP30     CLI   MODE,REQLAST                                                     
         BNE   TPEXT                                                            
         LA    R7,TAPEOUT                                                       
         CLOSE ((R7))              IF SO, CLOSE                                 
*                                                                               
         CP    TOTRECS,=PL6'0'     IS THERE DATA                                
         BNE   TP31A                                                            
         MVC   P(15),=C'NO DATA ON FILE'                                        
         B     TP32                                                             
*                                                                               
TP31A    MVC   P+1(23),=C'**TOTAL TAPE RECORDS = '                              
         EDIT  (P6,TOTRECS),(9,P+24),0,COMMAS=YES,ALIGN=LEFT                    
TP32     GOTO1 ACREPORT                                                         
         B     TPEXT                                                            
         EJECT                                                                  
*                                                                               
TPEXT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,RECFM=FB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=800,LRECL=160,BUFNO=2                                    
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
ACI9D    DSECT                     MY WORK                                      
TAPEREC  DS    0H                  HOLD FOR TAPE WRITE                          
TAPECODE DS    CL12                ACCOUNT CODE                                 
TAPENAME DS    CL36                ACCOUNT NAME                                 
TAPEADD1 DS    CL26                ADDRESS LINE 1                               
TAPEADD2 DS    CL26                ADDRESS LINE 2                               
TAPEADD3 DS    CL26                ADDRESS LINE 3                               
TAPEADD4 DS    CL26                ADDRESS LINE 4                               
TAPEFILL DS    CL8                                                              
TAPELEN  EQU   *-TAPEREC                                                        
*                                                                               
TOTRECS  DS    PL6                 TOTAL RECORDS                                
DSNAME   DS    CL20                TAPE NAME                                    
*ACGENBOTH                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPI902 09/25/00'                                      
         END                                                                    
