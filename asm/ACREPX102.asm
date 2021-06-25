*          DATA SET ACREPX102  AT LEVEL 033 AS OF 08/16/00                      
*PHASE ACX102A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ADD 12 BYTES TO CPOELDS/CONTRACT ORDER'                         
ACX102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX1**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX1D,RC                                                         
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   NCPOELDS,=P'0'      INIT # OF CPOELDS FIXED                      
         ZAP   NORDERS,=P'0'       INIT # OF ORDERS FIXED                       
*                                                                               
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
         XC    XKEY,XKEY                                                        
         LA    R3,XKEY                                                          
         USING ORDRECD,R3                                                       
         MVI   ORDKTYP,ORDKTYPQ    SET KEY FOR ORDER RECORDS                    
*                                                                               
         CLI   QCOMPANY,C' '       NO COMPANY?                                  
         BE    *+10                DO WHOLE ACCFILE                             
         MVC   ORDKCPY,QCOMPANY    JUST FOR THE COMPANY                         
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',XKEY,IO                          
         B     RS01                                                             
RS00     GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',XKEY,IO                          
RS01     LA    R3,IO                                                            
*                                                                               
RS03     CLI   ORDRECD,ORDKTYPQ    HAS TO BE ORDER RECORD                       
         BNE   RECAP               I WON'T BE BACK                              
*                                                                               
         CLI   QCOMPANY,C' '       NO COMPANY?                                  
         BE    *+14                DON'T CHECK FOR IT                           
         CLC   ORDKCPY,QCOMPANY    MATCHING COMPANY?                            
         BNE   RECAP                                                            
*                                                                               
         TM    ORDKSTAT,ORDSLDEL   LOGICALLY DELETED?                           
         BO    RS00                                                             
         TM    ORDKSTAT,ORDSCON    HAS TO BE A CONTRACT ORDER                   
         BZ    RS00                                                             
*                                                                               
         USING OOOELD,R4           OLD CPOELD                                   
RS05     LR    R4,R3                                                            
         MVI   ELCODE,CPOELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   RS00                                                             
         AP    NORDERS,=P'1'                                                    
RS10     LA    R5,ELEM                                                          
         USING CPOELD,R5                                                        
         XC    ELEM,ELEM                                                        
         MVC   CPOEL(OOOLN1Q),OOOEL                                             
         SR    R1,R1                                                            
         IC    R1,OOOLN                                                         
         SH    R1,=Y(OOOLN1Q)                                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CPODESC(0),OOODESC                                               
         ZAP   CPOQTYTD,=P'0'                                                   
         ZAP   CPOAMTTD,=P'0'                                                   
         MVI   OOOEL,X'FF'         MARK FOR DELETE                              
*                                                                               
         IC    R1,OOOLN                                                         
         LA    R1,12(R1)           ADD 12 BYTES                                 
         STC   R1,CPOLN            SAVE NEW LENGTH                              
         AP    NCPOELDS,=P'1'                                                   
         BAS   RE,SHOWIT                                                        
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),(X'FF',IO),0                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),IO,ELEM,0                         
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT1,C'P'          PRINT OUT INFO?                              
         BNE   RS20                                                             
         GOTO1 PRNTBL,DMCB,=C'REC BEF',IO,C'DUMP',2000,=C'1D'                   
         GOTO1 PRNTBL,DMCB,=C'(R4)',(R4),C'DUMP',96,=C'1D'                      
*                                                                               
RS20     BAS   RE,NEXTEL                                                        
         BE    RS10                FOUND ANOTHER, ADJUST IT                     
*                                                                               
RS30     CLI   QOPT1,C'P'          PRINT OUT INFO?                              
         BNE   RS50                                                             
         GOTO1 PRNTBL,DMCB,=C'REC AFT',IO,C'DUMP',2000,=C'1D'                   
RS50     CLI   RCWRITE,C'N'        TESTING?                                     
         BE    RS00                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',XKEY,IO                           
         B     RS00                                                             
         EJECT                                                                  
*                                                                               
RECAP    GOTO1 ACREPORT                                                         
         MVC   P(11),=C'** RECAP **'                                            
         GOTO1 ACREPORT                                                         
         MVC   P(27),=CL27'NUMBER OF FIXED CPOELDS:'                            
         EDIT  (P4,NCPOELDS),(5,P+28),ZERO=NOBLANK                              
         GOTO1 ACREPORT                                                         
         MVC   P(27),=CL27'NUMBER OF FIXED ORDERS: '                            
         EDIT  (P4,NORDERS),(5,P+28),ZERO=NOBLANK                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
         USING PLINE,R8                                                         
SHOWIT   NTR1                                                                   
         LA    R8,P                                                             
         MVC   PORDER,ORDKORD                                                   
         EDIT  CPOQTY,(L'PQTY,PQTY),ZERO=NOBLANK                                
         MVC   PSTOCK,CPOSTOCK                                                  
         SR    R1,R1                                                            
         IC    R1,CPOLN                                                         
         CH    R1,=Y(CPOLN1Q)                                                   
         BNH   SHOWIT5                                                          
         SH    R1,=Y(CPOLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   PDESC(0),CPODESC                                                 
SHOWIT5  EDIT  CPOUPRI,(L'PUNIT,PUNIT),2,ZERO=NOBLANK                           
         EDIT  CPOURET,(L'PRETAIL,PRETAIL),2,ZERO=NOBLANK                       
         EDIT  CPORATIO,(L'PRATIO,PRATIO),2,ZERO=NOBLANK                        
         EDIT  CPOQTYIN,(L'PQTYIVCD,PQTYIVCD),ZERO=NOBLANK                      
         EDIT  CPOAMTIN,(L'PAMTIVCD,PAMTIVCD),2,ZERO=NOBLANK                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
ITEMAMNT DC    PL6'0'                                                           
ITEMREF  DC    CL6' '                                                           
SAVEDATE DC    PL6'0'                                                           
NCPOELDS DC    PL4'0'                                                           
NORDERS  DC    PL4'0'                                                           
FOUND    DC    C' '                                                             
LSTFLDN  DC    X'00'                                                            
*                                                                               
SDATE    DC    XL2'00'                                                          
EDATE    DC    XL2'00'                                                          
CSDATE   DC    XL2'00'                                                          
CEDATE   DC    XL2'00'                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
DATVAL   DC    V(DATVAL)                                                        
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
         EJECT                                                                  
HEXIN    DC    V(HEXIN)                                                         
         LTORG                                                                  
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PORDER   DS    CL6                 ORDER                                        
         DS    CL3                                                              
PQTY     DS    CL5                 QUANTITY                                     
         DS    CL2                                                              
PSTOCK   DS    CL8                 STOCK NUMBER                                 
         DS    CL2                                                              
PDESC    DS    CL25                DESCRIPTION                                  
         DS    CL3                                                              
PUNIT    DS    CL12                UNIT PRICE                                   
         DS    CL2                                                              
PRETAIL  DS    CL12                RETAIL PRICE                                 
         DS    CL2                                                              
PRATIO   DS    CL8                 RATIO                                        
         DS    CL2                                                              
PQTYIVCD DS    CL5                 QUANTITY INVOICED                            
         DS    CL2                                                              
PAMTIVCD DS    CL12                AMOUNT INVOICED                              
*                                  ** OLD DSECT                                 
OOOELD   DSECT                     ** CONTRACT PURCHASE ORDER ELEM **           
OOOEL    DS    XL1                 ELEMENT CODE                                 
OOOELQ   EQU   X'94'                                                            
OOOLN    DS    XL1                 LENGTH                                       
OOOLINE  DS    XL1                 LINE NUMBER                                  
OOOQTY   DS    PL3                 QUANTITY                                     
OOOSTOCK DS    CL8                 STOCK NUMBER                                 
OOOUPRI  DS    PL6                 UNIT PRICE                                   
OOOURET  DS    PL6                 UNIT RETAIL                                  
OOORATIO DS    PL4                 RATIO FOR RETAIL EXTENSION                   
OOOQTYIN DS    PL3                 QUANTITY INVOICED                            
OOOAMTIN DS    PL6                 AMOUNT INVOICED                              
         DS    CL10                N/D                                          
OOOLN1Q  EQU   *-OOOELD                                                         
OOODESC  DS    0CL25               DESCRIPTION                                  
*                                                                               
ACX1D    DSECT                                                                  
ELCODE   DS    XL1                                                              
DATESAV  DS    CL16                                                             
DATELEN  DS    XL1                                                              
ELEM     DS    CL255                                                            
MNFLKEY  DS    CL42                                                             
XKEY     DS    CL42                                                             
PRVKEY   DS    CL42                                                             
TEXTMSG  DS    CL12                                                             
IO       DS    CL2100                                                           
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACREPX102 08/16/00'                                      
         END                                                                    
