*          DATA SET SPLDEXTPDE AT LEVEL 054 AS OF 12/12/03                      
*PHASE SPLDPDEL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'DELETE PRODUCT AND PRODUCT ESTIMATES'                           
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
*                                                                               
         CLI   0(R6),X'00'                                                      
         BNE   DMXKEEP                                                          
*                                                                               
* MUST BE SAME AGY/MEDIA AND CLIENT                                             
*                                                                               
         CLI   1(R6),X'E3'         AGENCY/MEDIA = GMMNY                         
         BNE   DMXKEEP                                                          
*                                                                               
         LA    RF,PRDLIST1         PG1 PRODUCT LIST                             
         CLC   2(2,R6),=XL2'9991'  CLIENT = GMR                                 
         BE    DMX10                                                            
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMX10    DS    0H                                                               
         ST    RF,APRDLIST                                                      
*                                                                               
         OC    4(9,R6),4(R6)      CLIENT RECORD?                                
         BNZ   DMX50              NO - IT'S A PRODUCT RECORD                    
*                                                                               
         L     R6,AREC                                                          
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   P(6),=C'BEFORE'                                                  
         GOTO1 VCLUNPK,DMCB,CKEYCLT,P+8    PRINT CLIENT                         
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+80                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+160                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+240                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+320                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+400                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+480                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+560                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+640                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+720                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+800                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R6,CLIST                                                         
         LA    R5,220             MAX # OF PRODUCTS                             
         L     R4,APRDLIST                                                      
*                                                                               
DMX20    DS    0H                                                               
         CLI   0(R4),X'FF'        NOT A FILTERING PRODUCT?                      
         BE    DMX40                                                            
*                                                                               
         CLC   0(3,R6),0(R4)      SAME PRODUCT CODE?                            
         BE    DMX30              YES - REMOVE IT FROM LIST                     
*                                                                               
         LA    R4,3(R4)           BUMP TO NEXT FILTERING PRODUCT                
         B     DMX20                                                            
*                                                                               
DMX30    DS    0H                                                               
         XC    0(4,R6),0(R6)      CLEAR OUT PRODUCT IN CLIENT RECORD            
*                                                                               
DMX40    DS    0H                                                               
         L     R4,APRDLIST                                                      
         LA    R6,4(R6)                                                         
         BCT   R5,DMX20                                                         
*                                                                               
         LA    RE,NEWCLIST                                                      
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         LA    R4,NEWCLIST                                                      
         LA    R5,220                                                           
         L     R6,AREC                                                          
         LA    R6,CLIST                                                         
*                                                                               
DMX45    DS    0H                                                               
         OC    0(4,R6),0(R6)       BLANK ENTRY?                                 
         BZ    DMX48                                                            
*                                                                               
         MVC   0(4,R4),0(R6)       MOVE INTO NEW CLIST                          
         LA    R4,4(R4)                                                         
DMX48    LA    R6,4(R6)                                                         
         BCT   R5,DMX45                                                         
*                                                                               
         L     R6,AREC                                                          
         LA    R4,NEWCLIST                                                      
*                                                                               
         LA    R6,CLIST            COPY NEW CLIENT LIST TO CLIENT REC           
         MVC   0(220,R6),0(R4)                                                  
         MVC   220(220,R6),220(R4)                                              
         MVC   440(220,R6),440(R4)                                              
         MVC   660(220,R6),660(R4)                                              
*                                                                               
         L     R6,AREC                                                          
         MVC   P(5),=C'AFTER'                                                   
         GOTO1 VCLUNPK,DMCB,CKEYCLT,P+8    PRINT CLIENT                         
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+80                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+160                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+240                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+320                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+400                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+480                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+560                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+640                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+720                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(80),CLIST+800                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
DMX50    DS    0H                                                               
         L     R6,AREC                                                          
         USING ESTRECD,R6                                                       
*                                                                               
* COMPARE PRODUCT CODES HERE, PURGE IF EQUAL                                    
*                                                                               
         L     R5,APRDLIST                                                      
DMX60    DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    DMXKEEP                                                          
*                                                                               
         CLC   4(3,R6),0(R5)       PRODUCT MATCH?                               
         BE    DMX70                                                            
         LA    R5,3(R5)            BUMP TO NEXT PRD IN LIST                     
         B     DMX60                                                            
*                                                                               
DMX70    DS    0H                                                               
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
*                                                                               
         GOTO1 VCLUNPK,DMCB,EKEYCLT,P      PRINT CLIENT                         
         MVC   P+5(3),EKEYPRD                       PRODUCT                     
         EDIT  EKEYEST,(3,P+10),ZERO=NOBLANK                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXPURGE                                                         
         DROP  R6                                                               
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
PRDLIST1 DC    C'BB '                                                           
         DC    C'BS '                                                           
         DC    C'BG '                                                           
         DC    C'BT '                                                           
         DC    C'BC '                                                           
         DC    C'BL '                                                           
         DC    C'BP '                                                           
         DC    C'BR '                                                           
         DC    C'BZ '                                                           
         DC    C'BZA'                                                           
         DC    C'MM '                                                           
         DC    C'MC '                                                           
         DC    C'ME '                                                           
         DC    C'TT '                                                           
         DC    C'TB '                                                           
         DC    C'TBA'                                                           
         DC    C'CC '                                                           
         DC    C'CA '                                                           
         DC    C'CIO'                                                           
         DC    C'CL '                                                           
         DC    C'CBO'                                                           
         DC    C'CT '                                                           
         DC    C'CP '                                                           
         DC    C'TP '                                                           
         DC    C'TD '                                                           
         DC    C'TK '                                                           
         DC    C'TKO'                                                           
         DC    C'CV0'                                                           
         DC    C'RC '                                                           
         DC    C'RCA'                                                           
         DC    C'RUN'                                                           
         DC    C'DA '                                                           
         DC    C'GXA'                                                           
         DC    C'GF '                                                           
         DC    C'GFA'                                                           
         DC    C'GJ '                                                           
         DC    C'GH '                                                           
         DC    C'GN '                                                           
         DC    C'GD '                                                           
         DC    C'GDA'                                                           
         DC    C'GX '                                                           
         DC    C'LA '                                                           
         DC    C'LAA'                                                           
         DC    C'LB '                                                           
         DC    C'LBA'                                                           
         DC    C'LUN'                                                           
         DC    C'OP '                                                           
         DC    C'PFX'                                                           
         DC    C'PR '                                                           
         DC    C'PX '                                                           
         DC    C'SUN'                                                           
         DC    X'FF'                                                            
*                                                                               
COUNT    DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
FLAG     DS    X                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
APRDLIST DS    A                                                                
SVAM     DS    X                                                                
SVCLT    DS    CL2                                                              
ESTTBL   DC    256X'FF'                                                         
ESTTBLQ  EQU   *-ESTTBL                                                         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
VCLUNPK  DS    A                                                                
*                                                                               
NEWCLIST DS    XL1000                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPLDEXTPDE12/12/03'                                      
         END                                                                    
