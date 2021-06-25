*          DATA SET DELDSPLT   AT LEVEL 003 AS OF 01/13/87                      
*PHASE DELDSPLT,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
DELDSPLT TITLE '- TAPE TO TAPE SPLIT OF EXTRA SPILL FROM ALL ELSE'              
DELDSPLT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,DMLDSPLT,=V(REGSAVE)                                         
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (IN,(INPUT))        INPUT TAPE                                   
*                                                                               
         OPEN  (OUT1,(OUTPUT))     EVERYTHING BUT SPILL TAPE                    
*                                                                               
         OPEN  (OUT2,(OUTPUT))     EXTRA SPILL TAPE                             
*                                                                               
         EJECT                                                                  
GETTAPE  EQU   *                                                                
         GET   IN,INREC                                                         
         L     R1,RECIN                                                         
         LA    R1,1(R1)            INPUT RECORD COUNT                           
         ST    R1,RECIN                                                         
***      LR    R3,R1               ***TEMP***                                   
         LH    R1,INREC                                                         
         A     R1,RECIN+4          INPUT RECORD LENGTH                          
         ST    R1,RECIN+4                                                       
***      CH    R3,=H'16000'        ***TEMP***                                   
***      BH    ENDTAPE             ***TEMP***                                   
         LA    R3,INREC+4          R3=A(INREC+4) POINT PAST 4-BYTE HEAD         
*                                                                               
         USING BSKEY,R3            STATION LIST RECORD                          
GT200    CLI   BSCODE,BSCODEQU                                                  
         BNE   GT300                                                            
         CLI   BSIND,BSINDEQU                                                   
         BNE   GT300                                                            
*                                                                               
***      B     GTXXX               ***TEMP***                                   
         LA    R2,RECOBS                                                        
         CLI   BSMEDIA,C'T'        (U.S.)                                       
         BE    *+12                                                             
         CLI   BSMEDIA,C'C'        (CANADA)                                     
         BNE   PUTOUT1                                                          
         CLI   BSBTYP,X'E0'        SPILL                                        
         BNE   PUTOUT1                                                          
         LA    R2,RECSBS                                                        
         B     PUTOUT2                                                          
*                                                                               
         SPACE 2                                                                
         USING MLKEY,R3            MARKET LIST RECORD                           
GT300    CLI   MLCODE,MLCODEQU                                                  
         BNE   GT400                                                            
*                                                                               
***      B     GTXXX               ***TEMP***                                   
         LA    R2,RECOML                                                        
         CLI   MLMEDIA,C'T'        (U.S.)                                       
         BE    *+12                                                             
         CLI   MLMEDIA,C'C'        (CANADA)                                     
         BNE   PUTOUT1                                                          
         CLI   MLBTYP,X'E0'        SPILL                                        
         BNE   PUTOUT1                                                          
         LA    R2,RECSML                                                        
         B     PUTOUT2                                                          
*                                                                               
         EJECT                                                                  
         USING DRKEY,R3            DEMO RECORDS                                 
GT400    CLI   DRCODE,DRCODEQU     REGULAR DEMO RECORDS                         
         BE    *+12                                                             
         CLI   DRCODE,C'D'         RADIO DAYPART RECORDS                        
         BNE   GT500                                                            
*                                                                               
         LA    R2,RECODR                                                        
         CLI   DRMEDIA,C'T'        (U.S.)                                       
         BE    *+12                                                             
         CLI   DRMEDIA,C'C'        (CANADA)                                     
         BNE   PUTOUT1                                                          
         CLI   DRBTYP,X'E0'        SPILL                                        
         BNE   PUTOUT1                                                          
         LA    R2,RECSDR                                                        
         B     PUTOUT2                                                          
*                                                                               
         SPACE 2                                                                
         USING SBKEY,R3            STATION/BOOK LIST RECORDS                    
GT500    CLI   SBCODE,SBCODEQU                                                  
         BNE   GT600                                                            
*                                                                               
         LA    R2,RECOSB                                                        
         CLI   SBMEDIA,C'T'        (U.S.)                                       
         BE    *+12                                                             
         CLI   SBMEDIA,C'C'        (CANADA)                                     
         BNE   PUTOUT1                                                          
         CLI   SBBTYP,X'E0'        SPILL                                        
         BNE   PUTOUT1                                                          
         LA    R2,RECSSB                                                        
         B     PUTOUT2                                                          
*                                                                               
         EJECT                                                                  
         USING UKEY,R3             UNIVERSE RECORDS                             
GT600    CLI   UCODE,UCODEQU                                                    
         BNE   GTXXX                                                            
*                                                                               
         LA    R2,RECOU                                                         
         CLI   UMEDIA,C'T'         (U.S.)                                       
         BE    *+12                                                             
         CLI   UMEDIA,C'C'         (CANADA)                                     
         BNE   PUTOUT1                                                          
         CLI   UBTYP,X'E0'         SPILL                                        
         BNE   PUTOUT1                                                          
         LA    R2,RECSU                                                         
         B     PUTOUT2                                                          
*                                                                               
         SPACE 2                                                                
*PUTOUT1 PUT   OUT1,INREC                                                       
PUTOUT1  L     R1,RECOTHER                                                      
         LA    R1,1(R1)            OUTPUT RECORD COUNT-NON-SPILL                
         ST    R1,RECOTHER                                                      
*                                                                               
         LH    R1,INREC                                                         
         A     R1,RECOTHER+4       OUTPUT RECORD LENGTH                         
         ST    R1,RECOTHER+4                                                    
*                                                                               
         L     R1,0(R2)                                                         
         LA    R1,1(R1)            INDIVIDUAL OUTPUT RECORD COUNT-NON-S         
         ST    R1,0(R2)                                                         
*                                                                               
         LH    R1,INREC                                                         
         A     R1,4(R2)            INDIVIDUAL OUTPUT RECORD LENGTH              
         ST    R1,4(R2)                                                         
         B     GETTAPE                                                          
*                                                                               
         SPACE 2                                                                
*PUTOUT2 PUT   OUT2,INREC                                                       
PUTOUT2  L     R1,RECSPILL                                                      
         LA    R1,1(R1)            OUTPUT RECORD COUNT-SPILL                    
         ST    R1,RECSPILL                                                      
*                                                                               
         LH    R1,INREC                                                         
         A     R1,RECSPILL+4       OUTPUT RECORD LENGTH                         
         ST    R1,RECSPILL+4                                                    
*                                                                               
         L     R1,0(R2)                                                         
         LA    R1,1(R1)            INDIVIDUAL OUTPUT RECORD COUNT-SPILL         
         ST    R1,0(R2)                                                         
***      LR    R3,R1               ***TEMP***                                   
*                                                                               
         LH    R1,INREC                                                         
         A     R1,4(R2)            INDIVIDUAL OUTPUT RECORD LENGTH              
         ST    R1,4(R2)                                                         
***      CH    R3,=H'10'           ***TEMP***                                   
***      BNL   ENDTAPE             ***TEMP***                                   
GTXXX    B     GETTAPE                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
ENDTAPE  EQU   *                                                                
         CLOSE (IN)                                                             
         CLOSE (OUT1)                                                           
         CLOSE (OUT2)                                                           
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+10(32),=C'EXTRA SPILL SPLIT SUMMARY TOTALS'                    
         MVC   P+44(08),=C'AVG RECD'                                            
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(31),P+10                                                    
         MVC   P+45(06),=C'LENGTH'                                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          COUNTER                                      
         LA    R3,BUCKTAB          POINTER TO BUCKETS                           
ENDTAPE4 MVC   P+10(20),8(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            RECORD TOTAL                                 
         EDIT  (R2),(10,P+32)      ------------                                 
*                                                                               
         SR    RE,RE                                                            
         L     RF,4(R3)            LENGTH TOTAL                                 
         LTR   RF,RF                                                            
         BZ    ENDTAPE8                                                         
         DR    RE,R2               DIVIDE TOTAL LENGTH BY TOTAL RECORDS         
         SLL   RE,1                                                             
         CR    RE,R2                                                            
         BL    *+8                                                              
         LA    RF,1(RF)            ROUND                                        
         LR    R2,RF               AVERAGE RECORD LENGTH                        
         EDIT  (R2),(6,P+44)       ---------------------                        
*                                                                               
ENDTAPE8 GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,ENDTAPE4                                                      
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
DMXIT    XIT1                                                                   
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
* ROUTINE TABLE                                                                 
*                                                                               
VPRINTER DC    V(PRINTER)                                                       
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL28                                                            
RECIN    DC    2F'0',CL20'RCDS IN'                                              
RECOTHER DC    2F'0',CL20'RCDS OUT(NON-SPILL)'                                  
RECOBS   DC    2F'0',CL20'     STATION LIST'                                    
RECOML   DC    2F'0',CL20'      MARKET LIST'                                    
RECODR   DC    2F'0',CL20'      DEMOGRAPHIC'                                    
RECOSB   DC    2F'0',CL20'    STA/BOOK LIST'                                    
RECOU    DC    2F'0',CL20'         UNIVERSE'                                    
RECSPILL DC    2F'0',CL20'RCDS OUT(SPILL)'                                      
RECSBS   DC    2F'0',CL20'     STATION LIST'                                    
RECSML   DC    2F'0',CL20'      MARKET LIST'                                    
RECSDR   DC    2F'0',CL20'      DEMOGRAPHIC'                                    
RECSSB   DC    2F'0',CL20'    STA/BOOK LIST'                                    
RECSU    DC    2F'0',CL20'         UNIVERSE'                                    
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
*                                                                               
         SPACE 2                                                                
DUB      DS    D                                                                
HALF     DS    H                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
WORK     DS    CL60                                                             
INREC    DS    CL2000              RECORD                                       
*                                                                               
         EJECT                                                                  
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               EODAD=ENDTAPE,                                          X        
               RECFM=VB,                                               X        
               LRECL=02000,                                            X        
               MACRF=GM                                                         
*                                                                               
OUT1     DCB   DDNAME=XSPILL1,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02000,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
OUT2     DCB   DDNAME=XSPILL2,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02000,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DELDSPLT  01/13/87'                                      
         END                                                                    
