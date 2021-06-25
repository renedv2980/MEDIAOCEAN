*          DATA SET ACBAT23    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T61B23A                                                                  
         TITLE 'OUTLET BILLING '                                                
T61B23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT23*,R8,CLEAR=YES                                
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         SR    R6,R6               NO PROFILES                                  
         EJECT                                                                  
*              DIG OUT ADVERTISER VALUES                                        
         SPACE 2                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'3'                                                       
         LA    R2,RETADVH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY+2(1),RETADV                                                  
         MVI   ERRNUM,17                                                        
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         LA    R4,KEY                                                           
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         BE    RT6                                                              
         B     ERROR                                                            
         USING ACHEIRD,R4                                                       
RT6      MVC   SAVELVB,ACHRLEVB                                                 
         DROP  R4                                                               
*                                                                               
RT10     MVC   CULSAVE,KEY         SAVE COMPANY,UNIT AND LEDGER                 
         SR    R1,R1                                                            
         IC    R1,SAVELVB                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),=12C'*'    ADVERTISER ACCOUNT IS ASTERISKS FOR          
*                                  LEVELS 1 AND 2 , AND ONE ASTERISK            
*                                  AT THE BOTTOM LEVEL                          
         BAS   RE,GETACC                                                        
         MVC   CONTROL,KEY                                                      
         MVC   CONTROLN,ACCTNAME                                                
         TM    RETADVH+4,X'20'                                                  
         BO    RT12                                                             
         OI    RETADVH+4,X'20'                                                  
         MVC   RETADVN,CONTROLN                                                 
         FOUT  RETADVNH                                                         
RT12     DC    0H'0'                                                            
         EJECT                                                                  
*              SAVE HEADLINE VALUES                                             
         SPACE 2                                                                
         LA    R2,RETMEDH                                                       
         MVI   ERRNUM,2                                                         
         MVI   MEDSW,C'N'                                                       
         BAS   RE,ANY                                                           
         BAS   RE,MEDLOOK                                                       
         CLI   MEDSW,C'Y'                                                       
         BNE   ERROR                                                            
         SPACE 1                                                                
         LA    R2,RETPROH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES          VALIDATE PRODUCT (AS WORK-CODE)              
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CULSAVE                                                 
         MVC   KEY+4(2),RETPRO                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         SPACE 1                                                                
         LA    R2,RETSCH                                                        
         BAS   RE,ANY                                                           
         MVC   KEY+4(2),RETSC      VALIDATE SCHEME                              
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   KEY(3),CULSAVE                                                   
         SPACE 1                                                                
         LA    R2,RETMOSH                                                       
         BAS   RE,ANY                                                           
         GOTO1 DATVAL,DMCB,(2,RETMOS),WORK                                      
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,DATE)                                    
         SPACE 1                                                                
         MVC   NARR,SPACES                                                      
         MVI   NARRLEN,0                                                        
         CLI   RETNARRH+5,0                                                     
         BE    RT20                                                             
         ZIC   RF,RETNARRH+5                                                    
         STC   RF,NARRLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NARR(0),RETNARR                                                  
         EJECT                                                                  
*              NOW VALIDATE/SAVE INPUT BILL DETAILS                             
         SPACE 2                                                                
RT20     DS    0H                                                               
         LA    R2,RETFMKTH                                                      
         BAS   RE,ANY                                                           
         LA    R2,RETFCSHH                                                      
         BAS   RE,ANY                                                           
         LA    R2,RETFMKTH                                                      
         ZAP   LINES,=P'11'                                                     
         MVC   SAVEMKT,SPACES                                                   
         LA    R5,BILLTAB                                                       
         USING BILLD,R5                                                         
         SPACE 1                                                                
RT22     DS    0H                                                               
         MVC   BILLACC,CONTROL+3   BUILD TABLE ENTRY FOR                        
         MVC   BILLACCN,CONTROLN   CONTROL POSTING FIRST                        
         ZIC   RF,0(R2)                                                         
         LR    R3,R2                                                            
         AR    R3,RF                                                            
         MVI   ERRNUM,25                                                        
         CLI   5(R3),0                                                          
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     ERROR                                                            
         ZIC   R4,5(R3)                                                         
         GOTO1 AMTVAL,DMCB,8(R3),(R4)                                           
         CLI   0(R1),X'FF'                                                      
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     ERROR                                                            
         L     RF,DMCB+4                                                        
         ZAP   DUB,0(8,RF)                                                      
         ZAP   SAVECASH,DUB                                                     
         ZAP   BILLCASH,DUB                                                     
         ZAP   LEFTCASH,DUB                                                     
         SPACE 1                                                                
*                                  NOW THE DESCRIPTION                          
*                                                                               
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         IC    RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RE,NARRLEN                                                       
         MVC   BILLNARR,SPACES                                                  
         MVC   BILLNARL(31),NARRLEN                                             
         CLI   5(R3),0                                                          
         BE    RT24                                                             
         IC    RF,5(R3)                                                         
         AR    RF,RE                                                            
         STC   RF,BILLNARL                                                      
         LA    RE,BILLNARR(RE)                                                  
         IC    RF,5(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R3)                                                    
         EJECT                                                                  
*                                                                               
*                                  NOW DISTRIBUTE                               
RT24     DS    0H                                                               
         MVC   KEY(3),CULSAVE                                                   
         MVC   KEY+3(12),SAVEMKT   IF NO MARKET - USE PREVIOUS                  
         CLI   5(R2),0                                                          
         BE    RT25                                                             
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         MVC   SAVEMKT,KEY+3                                                    
RT25     DS    0H                                                               
         MVI   ERRNUM,17                                                        
         BAS   RE,HIGH                                                          
         LA    R4,KEY                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVI   ELCODE,X'62'        FIND SCHEME ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING ACDISTD,R4                                                       
RT26     CLC   RETSC,ACDICODE                                                   
         BE    RT28                                                             
         BAS   RE,NEXTEL                                                        
         BE    RT26                                                             
         B     ERROR                                                            
         SPACE 1                                                                
RT28     DS    0H                                                               
         ZAP   MKTUNITS,ACDIVAL                                                 
         ZAP   LEFTUNTS,ACDIVAL                                                 
         ZIC   RF,0(R2)                                                         
         LR    R3,R2                                                            
         AR    R3,RF                                                            
         IC    RF,0(R3)                                                         
         AR    R3,RF               POINT TO OVERRIDE                            
         MVI   ERRNUM,1                                                         
         CLI   5(R3),0                                                          
         BNE   RT29                                                             
         LR    R2,R3                                                            
         B     RT38                NEED NOT HAVE OVERRIDE OUTLET                
*                                                                               
RT29     DC    0H'0'                                                            
         MVC   BILLOVER,8(R3)                                                   
         OC    BILLOVER,SPACES                                                  
         CLI   8(R3),X'7B'         NUMBER SIGN - MEANS ALL TO ONE ACCT.         
         BNE   RT30                                                             
         IC    RF,5(R3)                                                         
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         ZIC   RE,SAVELVB                                                       
         LA    R1,KEY+3(RE)                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),9(R3)                                                    
         LR    R2,R3                                                            
         BAS   RE,GETACC                                                        
         LA    R5,L'BILLEN(R5)                                                  
         MVC   BILLACC,ACCTNUM+3                                                
         MVC   BILLACCN,ACCTNAME                                                
         ZAP   BILLCASH,SAVECASH                                                
         B     RT56                                                             
         SPACE 3                                                                
RT30     LA    R5,L'BILLEN(R5)     PART TO CO-OPPER                             
         MVC   SAVEKEY,KEYSAVE     REST TO SPECIFIC OUTLET                      
         IC    RF,5(R3)                                                         
         BCTR  RF,0                                                             
         ZIC   RE,SAVELVB                                                       
         LA    R1,KEY+3(RE)                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(R3)                                                    
         LR    R2,R3                                                            
         BAS   RE,GETACC           VALIDATE OVERRIDE OUTLET                     
         MVC   SVACC,ACCTNUM+3                                                  
         MVC   SVACCN,ACCTNAME                                                  
         SPACE 1                                                                
         MVC   KEY,SAVEKEY                                                      
         ZIC   RE,SAVELVB                                                       
         LA    R4,KEY+3(RE)                                                     
         MVI   0(R4),C'*'                                                       
         BAS   RE,HIGH                                                          
         CLI   0(R4),C'*'                                                       
         BNE   ERROR                                                            
         LA    R4,KEY                                                           
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
         USING ACDISTD,R4                                                       
RT32     CLC   RETSC,ACDICODE                                                   
         BE    RT34                                                             
         BAS   RE,NEXTEL                                                        
         BE    RT32                                                             
         B     ERROR                                                            
         SPACE 1                                                                
RT34     ZAP   DIV,SAVECASH                                                     
         MP    DIV,ACDIVAL         GIVE CO-OPPER HIS SHARE                      
         DP    DIV,MKTUNITS                                                     
         ZAP   BILLCASH,DIV(8)                                                  
         SP    SAVECASH,BILLCASH                                                
         MVC   BILLACC,KEY+3                                                    
         BAS   RE,GETACC                                                        
         MVC   BILLACCN,ACCTNAME                                                
         SPACE 1                                                                
         LA    R5,L'BILLEN(R5)     NOW THE SPECIFIC OUTLET                      
         MVC   BILLACC,SVACC                                                    
         MVC   BILLACCN,SVACCN                                                  
         ZAP   BILLCASH,SAVECASH                                                
         B     RT56                                                             
         SPACE 3                                                                
*                                  DISTRIBUTE BILL TO ALL OUTLETS               
*                                  IN THIS MARKET                               
RT38     ZAP   DAYCOUNT,=P'0'                                                   
RT40     BAS   RE,SEQ                                                           
         ZIC   RE,SAVELVB                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   RT56                                                             
         SPACE 1                                                                
         CLC   KEY+15(17),SPACES   ACCOUNT?                                     
         BNE   RT46                                                             
         LA    R4,KEY                                                           
         MVI   ELCODE,X'62'                                                     
         BAS   RE,GETEL                                                         
         BNE   RT46                                                             
         USING ACDISTD,R4                                                       
RT42     CLC   RETSC,ACDICODE                                                   
         BE    RT44                                                             
         BAS   RE,NEXTEL                                                        
         BE    RT42                                                             
         B     RT46                                                             
         SPACE 1                                                                
RT44     LA    R5,L'BILLEN(R5)                                                  
         AP    DAYCOUNT,=P'1'                                                   
         CP    DAYCOUNT,=P'8'                                                   
         BL    *+12                                                             
         MVI   ERRNUM,70           ACCDAY RECORD TOO BIG                        
         B     ERROR                                                            
         SP    LEFTUNTS,ACDIVAL                                                 
         CP    LEFTUNTS,=P'0'                                                   
         BH    *+14                                                             
         ZAP   BILLCASH,LEFTCASH                                                
         B     RT45                                                             
         ZAP   DIV,SAVECASH                                                     
         MP    DIV,ACDIVAL                                                      
         DP    DIV,MKTUNITS                                                     
         ZAP   BILLCASH,DIV(8)                                                  
         SP    LEFTCASH,BILLCASH                                                
RT45     DS    0H                                                               
         MVC   BILLACC,KEY+3                                                    
         BAS   RE,GETACC                                                        
         MVC   BILLACCN,ACCTNAME                                                
         SPACE 1                                                                
RT46     DC    0H'0'                                                            
         ZIC   RF,KEY+14                                                        
         LA    RF,1(RF)                                                         
         STC   RF,KEY+14                                                        
         B     RT40                                                             
         SPACE 3                                                                
RT56     DS    0H                                                               
         LA    R5,L'BILLEN(R5)                                                  
         LR    R2,R3                                                            
         ZIC   RF,0(R3)                                                         
         AR    R2,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         SP    LINES,=P'1'                                                      
         CP    LINES,=P'0'         ANY MORE TO LOOK AT                          
         BNE   *+12                                                             
         MVI   0(R5),X'FF'                                                      
         B     RT60                                                             
         CLI   5(R2),0             ANY INPUT ON THIS ONE                        
         BNE   RT22                                                             
         ZIC   RF,0(R2)            IF NO MARKET - WE MAY HAVE CASH              
         LR    R3,R2                                                            
         AR    R3,RF                                                            
         CLI   5(R3),0                                                          
         BNE   RT22                                                             
         MVI   0(R5),X'FF'                                                      
         B     RT60                                                             
         EJECT                                                                  
*              NOW BUILD SOME POSTING RECORDS                                   
         SPACE 2                                                                
RT60     LA    R5,BILLTAB                                                       
RT62     LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         USING BILLD,R5                                                         
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSDATE,DATE                                                    
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSNARR,C' '                                                    
         LA    RF,1                                                             
         CLI   BILLNARL,0                                                       
         BE    RT64                                                             
         ZIC   RF,BILLNARL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),BILLNARR                                             
         LA    RF,1(RF)                                                         
RT64     LA    RE,DLDSNARR                                                      
         SR    RE,R7                                                            
         AR    RE,RF                                                            
         STC   RE,DLDSLEN                                                       
         SPACE 2                                                                
         AR    R7,RE               DO A 23 AND 6A                               
         USING ACOTHERD,R7                                                      
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(13),SPACES                                               
         MVC   ACOTNUM(8),BILLOVER                                              
         OC    ACOTNUM(8),SPACES                                                
         MVC   ACOTPROF(2),RETSC                                                
         IC    RE,ACOTLEN                                                       
         AR    R7,RE                                                            
         SPACE 1                                                                
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDIT CONTROL ACCOUNT(ASTERISKS)            
         MVC   DLPSCRAC(3),CULSAVE                                              
         MVC   DLPSCRAC+3(12),BILLACC                                           
         MVC   DLPSCRNM,BILLACCN                                                
         MVC   DLPSANAL,RETPRO                                                  
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBAC+1(2),RETMED                                             
         MVC   DLPSDBAC+4(2),RETPRO                                             
         MVC   DLPSDBNM,SPACES                                                  
         MVC   DLPSDBNM(15),MEDNAME                                             
         ZAP   DLPSAMNT,BILLCASH                                                
         ZAP   SAVECASH,BILLCASH                                                
         MVI   DLPSTYPE,0                                                       
         LA    R5,L'BILLEN(R5)                                                  
         SPACE 1                                                                
RT70     IC    RE,DLPSLEN                                                       
         AR    R7,RE                                                            
         USING TRCASHD,R7                                                       
         MVC   TRCSEL(2),=X'5009'  TOTAL BILL VALUE                             
         MVI   TRCSTYPE,C'G'                                                    
         ZAP   TRCSAMNT,SAVECASH                                                
         SPACE 1                                                                
         IC    RE,TRCSLEN                                                       
         AR    R7,RE                                                            
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6971'  DEBIT OUTLET                                 
         MVC   DLPSDBAC(3),CULSAVE                                              
         MVC   DLPSDBAC+3(12),BILLACC                                           
         MVC   DLPSDBNM,BILLACCN                                                
         MVC   DLPSCRAC,SPACES                                                  
         MVC   DLPSCRAC+1(2),RETMED                                             
         MVC   DLPSCRNM,SPACES                                                  
         MVC   DLPSCRNM(15),MEDNAME                                             
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,RETPRO                                                  
         ZAP   DLPSAMNT,BILLCASH                                                
         SPACE 1                                                                
         ST    R7,FULL                                                          
         LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVC   DLDSREF,BILLACC     FIRST PART OF REG/MKT TO REFERENCE           
         L     R7,FULL                                                          
         USING DLPOSTD,R7                                                       
         SPACE 2                                                                
         LA    R5,L'BILLEN(R5)                                                  
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    RT72                                                             
         CLC   BILLACC(5),=C'*****' OR NEXT BILL                                
         BNE   RT70                                                             
         SPACE 1                                                                
RT72     IC    RE,DLPSLEN                                                       
         AR    R7,RE                                                            
         MVI   0(R7),0                                                          
         LA    RE,IOAREA-1                                                      
         SR    R7,RE                                                            
         STH   R7,HALF                                                          
         MVC   IOAREA(2),HALF                                                   
         BAS   RE,PUTDAY                                                        
         XC    WORK,WORK                                                        
         LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         MVC   WORK(6),DLDSREF     REF                                          
         L     RF,DMCB+8                                                        
         MVC   WORK+10(4),0(RF)    DISK ADDRESS                                 
         ZAP   DUB,SAVECASH                                                     
         ZAP   TRANSAMT,SAVECASH                                                
         BAS   RE,ADTWA1                                                        
         SPACE 1                                                                
         CLI   0(R5),X'FF'                                                      
         BNE   RT62                                                             
         SPACE 1                                                                
         LA    R2,RETADVH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              LOOK UP THE MEDIA TABLE                                          
         SPACE 2                                                                
MEDLOOK  NTR1                                                                   
         LA    R4,MEDIATAB                                                      
MD2      CLI   0(R4),X'FF'                                                      
         BE    MDEXIT                                                           
         CLC   0(2,R4),8(R2)                                                    
         BE    MDYES                                                            
         LA    R4,17(R4)                                                        
         B     MD2                                                              
         SPACE 1                                                                
MDYES    MVI   MEDSW,C'Y'                                                       
         MVC   MEDIA,8(R2)                                                      
         MVC   MEDNAME,2(R4)                                                    
MDEXIT   XIT1                                                                   
         SPACE 3                                                                
MEDIATAB DC    C'ST',CL15'SPOT TELEVISION'                                      
         DC    C'SR',CL15'SPOT RADIO'                                           
         DC    C'SN',CL15'NETWORK TV'                                           
         DC    C'SX',CL15'NETWORK RADIO'                                        
         DC    C'PN',CL15'NEWSPAPERS'                                           
         DC    C'PM',CL15'MAGAZINES'                                            
         DC    C'PS',CL15'SUPPLEMENTS'                                          
         DC    C'PT',CL15'TRADE'                                                
         DC    C'PO',CL15'OUTDOOR'                                              
         DC    C'PR',CL15'PRODUCTION'                                           
         DC    C'XX',CL15'MISCELLANEOUS'                                        
         DC    X'FF'                                                            
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATD8D                                                       
         EJECT                                                                  
*                                                                               
*                                                                               
PROGD    DSECT                                                                  
YES      DS    CL1                                                              
MEDIA    DS    CL2                                                              
CONTROL  DS    CL15                                                             
CONTROLN DS    CL36                                                             
DATE     DS    CL3                                                              
MEDSW    DS    CL1                                                              
MEDNAME  DS    CL15                                                             
LINES    DS    CL2                                                              
NARRLEN  DS    CL1                                                              
NARR     DS    CL30                                                             
SVACC    DS    CL15                                                             
SVACCN   DS    CL36                                                             
ELCODE   DS    CL1                                                              
SAVECASH DS    PL6                                                              
MKTUNITS DS    PL6                                                              
LEFTCASH DS    PL6                                                              
LEFTUNTS DS    PL6                                                              
DIV      DS    PL14                                                             
SAVEKEY  DS    CL42                                                             
SAVEMKT  DS    CL12                                                             
CULSAVE  DS    CL3                                                              
SAVELVB  DS    C                                                                
DAYCOUNT DS    C                                                                
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
BILLTAB  DS    3500C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*              DSECT FOR A LINE IN THE BILL TABLE                               
         SPACE 2                                                                
BILLD    DSECT                                                                  
BILLEN   DS    0CL123                                                           
BILLACC  DS    CL12                                                             
BILLACCN DS    CL36                                                             
BILLCASH DS    PL6                                                              
BILLNARL DS    CL1                                                              
BILLNARR DS    CL60                                                             
BILLOVER DS    CL8                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              ACGENBOTH                                                        
*              ACGENDAY                                                         
*              DDFLDIND                                                         
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACBAT23   05/01/02'                                      
         END                                                                    
