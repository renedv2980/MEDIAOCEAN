*          DATA SET ACLDXABL   AT LEVEL 137 AS OF 03/01/13                      
*PHASE ACLDXABL                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
***********************************************************************         
* EXTERNAL TO DEAL WITH HOLDING NUMBER OF TRANSACTIONS AT             *         
* LEDGER LEVEL, AND AT HIGH LEVEL AND LOW LEVEL ACCOUNTS              *         
*                                                                     *         
* (1) ADD NEW NUMELD TO LEDGER AND HIGH LEVEL ACCOUNT RECORDS         *         
* (2) EXTEND ABLELDS (TO INCLUDE ABLTXS AND 4 SPARE BYTES)            *         
*                                                                     *         
* USE JCL ACLDABL  (SMAN) FOR TESTING                                 *         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
ORG      DC    C'*VERIFY*020301020331',C'*',X'FF'                               
         ORG   ORG+8                                                            
         DC    X'FB'                                                            
         ORG   *+5                                                              
         DC    X'FB'                                                            
         ORG                                                                    
COMPANY  DS    X                                                                
DMXRTST  DS    0H                                                               
         B     DMXKEEP             KEEP WHEN LIVE                               
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXINIT  DS    0H                                                               
DMXRET   DS    0H                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'LEDGER NUMELS ADDED  = '                              
         CURED LDGLVL,(14,P+62),0,MINUS=YES                                     
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'HI LVL NUMELS ADDED  = '                              
         CURED HIGHLVL,(14,P+62),0,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'ABLELDS NOT CHANGED  = '                              
         CURED NOCHA,(14,P+62),0,MINUS=YES                                      
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'ABLELDS CHANGED      = '                              
         CURED CONVERTS,(14,P+62),0,MINUS=YES                                   
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'ABLELDS MISSING      = '                              
         CURED MISSING,(14,P+62),0,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
                                                                                
         SPACE                                                                  
         EJECT                                                                  
DMXREC   L     R2,VREC                                                          
         USING ACTRECD,R2                                                       
         GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
         CLI   RECTYPE,ACRTLDG     LEDGER LEVEL                                 
         BE    DMXREC10                                                         
         CLI   RECTYPE,ACRTACTH    HIGH LEVEL ACCOUNT RECORD                    
         BE    DMXREC20                                                         
         CLI   RECTYPE,ACRTACTL    LOW LEVEL ACCOUNT RECORD                     
         BE    DMXREC45                                                         
         B     DMXRTST                                                          
                                                                                
* LEDGER LEVEL ACCOUNT                                                          
DMXREC10 MVI   LDGAL1,0                                                         
         MVI   LDGAL2,0                                                         
         MVI   LDGAL3,0                                                         
         MVI   LDGAL4,0                                                         
         MVI   IND1,0                                                           
                                                                                
         USING ACLELD,R3                                                        
         LA    R3,ACTRFST                                                       
         XR    RE,RE                                                            
DMXREC12 CLI   ACLEL,0                                                          
         BNE   DMXREC13                                                         
         TM    IND1,ACLEFND                                                     
         BNZ   *+6                                                              
         DC    H'0'                ACLELD MISSING                               
         TM    IND1,NUMLFND                                                     
         BZ    DMXREC18            NUMELD NOT FOUND SO ADD ONE                  
         NI    IND1,X'FF'-NUMLFND                                               
         B     DMXKEEP                                                          
                                                                                
DMXREC13 CLI   ACLEL,ACLELQ                                                     
         BE    DMXREC15                                                         
         CLI   ACLEL,NUMELQ                                                     
         BE    DMXREC16                                                         
DMXREC14 IC    RE,ACLLN                                                         
         AR    R3,RE                                                            
         B     DMXREC12                                                         
                                                                                
DMXREC15 MVC   LDGAL1,ACLELLVA                                                  
         MVC   LDGAL2,ACLELLVB                                                  
         MVC   LDGAL3,ACLELLVC                                                  
         MVC   LDGAL4,ACLELLVD                                                  
         OI    IND1,ACLEFND                                                     
         B     DMXREC14                                                         
                                                                                
         USING NUMELD,R3                                                        
DMXREC16 CLI   NUMTYPE,NUMTYLEQ                                                 
         BNE   DMXREC14                                                         
         OI    IND1,NUMLFND                                                     
         B     DMXREC14                                                         
         DROP  R3                                                               
                                                                                
NEW      USING NUMELD,R4                                                        
DMXREC18 LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NEW.NUMEL,NUMELQ                                                 
         MVI   NEW.NUMTYPE,NUMTYLEQ    L FOR LEDGER                             
         AP    LDGLVL,=P'1'                                                     
         B     DMXREC40                                                         
                                                                                
* HIGH LEVEL ACCOUNT                                                            
DMXREC20 DS    0H                                                               
* CHECK IF ACTUALLY A LOW LEVEL ACCOUNT W/O AN ABLELD                           
         CLI   LDGAL1,L'ACTKACT                                                 
         BE    DMXREC45           (ALL SINGLE LEVEL ACCOUNTS ARE LOW)           
                                                                                
         LA    RE,L'ACTKACT                                                     
         LA    RF,ACTKACT+L'ACTKACT-1                                           
DMXREC21 CLI   0(RF),C' '                                                       
         BH    DMXREC22                                                         
         BCTR  RF,0                                                             
         BCT   RE,DMXREC21                                                      
         DC    H'0'                                                             
                                                                                
DMXREC22 STC   RE,BYTE          BYTE CONTAINS ACTUAL LENGTH OF ACTKACT          
         CLI   LDGAL4,L'ACTKACT                                                 
         BNE   DMXREC23                                                         
         CLC   BYTE,LDGAL3                                                      
         BH    DMXREC45            (LOW LEVEL ACCOUNT)                          
         B     DMXREC30                                                         
                                                                                
DMXREC23 CLI   LDGAL3,L'ACTKACT                                                 
         BNE   DMXREC24                                                         
         CLC   BYTE,LDGAL2                                                      
         BH    DMXREC45            (LOW LEVEL ACCOUNT)                          
         B     DMXREC30                                                         
                                                                                
DMXREC24 CLI   LDGAL2,L'ACTKACT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,LDGAL1                                                      
         BH    DMXREC45            (LOW LEVEL ACCOUNT)                          
                                                                                
DMXREC30 LA    R3,ACTRFST                                                       
         USING NUMELD,R3                                                        
         XR    R1,R1                                                            
DMXREC32 CLI   NUMEL,0                                                          
         BNE   DMXREC34                                                         
         TM    IND1,NUMHFND                                                     
         BZ    DMXREC38                                                         
         NI    IND1,X'FF'-NUMHFND                                               
         B     DMXKEEP                                                          
                                                                                
DMXREC34 CLI   NUMEL,NUMELQ                                                     
         BNE   DMXREC36                                                         
         CLI   NUMTYPE,NUMTYHIQ                                                 
         BNE   DMXREC36                                                         
         OI    IND1,NUMHFND                                                     
DMXREC36 IC    R1,NUMLN                                                         
         AR    R3,R1                                                            
         B     DMXREC32                                                         
         DROP  R3                                                               
                                                                                
DMXREC38 LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   NEW.NUMEL,NUMELQ                                                 
         MVI   NEW.NUMTYPE,NUMTYHIQ   H FOR HIGH LEVEL ACCOUNT                  
         AP    HIGHLVL,=P'1'                                                    
*        GOTO1 VPRINTER                                                         
*        MVC   P+1(14),ACTKULA                                                  
*        MVC   P+16(12),=C'NUMELD ADDED'                                        
*        GOTO1 VPRINTER                                                         
                                                                                
* ADD NUMELD                                                                    
DMXREC40 MVI   NEW.NUMLN,NUMLN2Q                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),(R2),ELEMENT,0                         
         CLI   12(R1),0                                                         
         BE    DMXKEEP                                                          
         DC    H'0'                                                             
         DROP  NEW                                                              
                                                                                
* LOW LEVEL ACCOUNT                                                             
DMXREC45 LA    R3,ACTRFST                                                       
         XR    RE,RE                                                            
         USING ABLELD,R3                                                        
DMXREC48 CLI   ABLEL,0                                                          
         BNE   DMXREC49                                                         
         AP    MISSING,=P'1'        MISSING ABLELD                              
         GOTO1 VPRINTER                                                         
         MVC   P+1(14),ACTKULA                                                  
         MVC   P+16(14),=C'MISSING ABLELD'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXREC49 CLI   ABLEL,ABLELQ                                                     
         BE    DMXREC50                                                         
         IC    RE,ABLLN                                                         
         AR    R3,RE                                                            
         B     DMXREC48                                                         
                                                                                
DMXREC50 CLI   ABLLN,ABLLN3Q                                                    
         BNE   *+14                                                             
         AP    NOCHA,=P'1'         NO CHANGE TO ABLELD                          
         B     DMXKEEP                                                          
         XC    ELEMENT,ELEMENT                                                  
         IC    RE,ABLLN                                                         
         BCTR  RE,0                                                             
         EXMVC RE,ELEMENT,ABLELD                                                
NEW      USING ABLELD,ELEMENT                                                   
         CLI   NEW.ABLLN,ABLLN1Q   TEST SHORT ELEMENT                           
         BE    DMXREC65                                                         
         CLI   NEW.ABLLN,ABLLN2Q                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    ABLURG,ABLURG                                                    
         BNZ   *+10                                                             
DMXREC65 ZAP   ABLURG,=P'0'                                                     
         MVI   NEW.ABLLN,ABLLN3Q                                                
                                                                                
         MVI   ABLEL,X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ACTRECD),0,0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ACTRECD,ELEMENT,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    CONVERTS,=P'1'        ABLELD CONVERTED                           
                                                                                
         B     DMXKEEP                                                          
         DROP  R3,NEW                                                           
                                                                                
         EJECT                                                                  
         LTORG                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
CUREDIT  DC    V(CUREDIT)                                                       
VHELLO   DC    V(HELLO)                                                         
ACCMST   DC    C'ACCMST  '                                                      
*                                                                               
CONVERTS DC    PL12'0'                                                          
MISSING  DC    PL12'0'                                                          
NOCHA    DC    PL12'0'                                                          
LDGLVL   DC    PL12'0'                                                          
HIGHLVL  DC    PL12'0'                                                          
LDGAL1   DS    XL1                                                              
LDGAL2   DS    XL1                                                              
LDGAL3   DS    XL1                                                              
LDGAL4   DS    XL1                                                              
IND1     DS    XL1                                                              
ACLEFND  EQU   X'80'               ACLELD FOUND                                 
NUMLFND  EQU   X'40'               NUMELD/NUMTYLEQ FOUND                        
NUMHFND  EQU   X'20'               NUMELD/NUMTYHEQ FOUND                        
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
BYTE     DS    XL1                                                              
*                                                                               
         DS    0F                                                               
PLIST    DS    0X                                                               
VREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    V                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
VLDDEF   DC    V(LDDEFN)                                                        
*                                                                               
RECTYPE  DS    X                                                                
ELEMENT  DS    XL256                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'137ACLDXABL  03/01/13'                                      
         END                                                                    
