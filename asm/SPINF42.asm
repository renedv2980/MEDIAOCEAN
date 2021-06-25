*          DATA SET SPINF42    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T21A42A                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'T21A42 - SPOTPAK INFO PROGRAM DEFINITION'                       
*                                                                               
*        SVKEY HAS FOLLOWING UPON ENTRY                                         
*              0-1  X'0D12'                                                     
*              2-3  ALPHA AGENCY                                                
*              4-7  NETWORK / ALL=ALL NETWORKS                                  
*              8-11 PROGRAM / ALL=ALL PROGRAMS                                  
*                                                                               
T21A42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 100,T21A42,RR=R8                                                 
         USING FLDHDRD,R2                                                       
         LR    R3,RC                                                            
         USING WRK42,R3                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
*                                                                               
         LA    R5,REC                                                           
         ST    R5,AREC                                                          
         ST    R8,RELO                                                          
*                                                                               
         LA    R2,SINHDRH                                                       
         LA    R4,SINHDR                                                        
         USING FLDDATAD,R4                                                      
*                                                                               
         MVC   FLDNTWK,=C'NTWK'                                                 
         MVC   FLDCLT,=C'CLT'                                                   
         MVC   FLDPROG,=C'CODE'                                                 
         MVC   FLDNAME(9),=C'PROG NAME'                                         
         MVC   FLDRTG(3),=C'RTG'                                                
         MVC   FLDSEQ(3),=C'SEQ'                                                
         MVC   FLDDATAL(FLDDATAL-1,R4),0(R4) SET FOR 2 COLS                     
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   FLDNTWK,DASH                                                     
         MVC   FLDCLT,DASH                                                      
         MVC   FLDPROG,DASH                                                     
         MVC   FLDNAME(9),DASH                                                  
         MVC   FLDRTG(3),DASH                                                   
         MVC   FLDSEQ(3),DASH                                                   
         MVC   FLDDATAL(FLDDATAL-1,R4),0(R4)                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LINLEN(R2)                                                    
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NPGMRECD,R5                                                      
         LA    RE,REC2                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         EJECT                                                                  
         XC    INPTAB,INPTAB       NUMBER OF LINES INPUT TO TABLE               
         LA    R7,REC2                                                          
         USING SVNPGMD,R7                                                       
*                                                                               
         MVC   NPGMKTYP,=X'0D12'                                                
         MVC   NPGMKAGY,SVKEY+2                                                 
         MVC   NPGMKNET,SVKEY+4                                                 
         MVC   NPGMKID,SVKEY+8                                                  
         OC    PREVKEY,PREVKEY     CONT. OF PREVIOUS TRANS                      
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
RHI      GOTO1 HIGH                                                             
         B     HAVREC                                                           
*                                                                               
RSEQ     GOTO1 SEQ                                                              
*                                                                               
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   NOMORE                                                           
         CLI   SVKEY+4,0                                                        
         BE    CHKKEY1                                                          
         CLC   SVKEY+4(3),=C'ALL'                                               
         BE    CHKKEY1                                                          
         CLC   SVKEY+4(4),=C'    ' NETWORK FILTER                               
         BE    CHKKEY1              NO                                          
         CLC   NPGMKNET,SVKEY+4     YES - FILTER PROPER NETWORK                 
         BNE   NOMORE                                                           
*                                                                               
CHKKEY1  MVC   SVNTWK,NPGMKNET     MOVE KEY DATA TO SAVE AREA                   
         MVC   SVPROG,NPGMKID                                                   
         GOTO1 GETREC              READ THE RECORD                              
         L     RE,AREC                                                          
         MVC   SVNAME,NPGMPGM-NPGMRECD(RE)                                      
*                                                                               
         MVC   LASTKEY,KEY                                                      
         BAS   RE,RDOVR            GET OVERRIDE RECORDS                         
         BE    CHKKEY20                                                         
         LA    R7,SVNPGMX-SVNPGMD(R7)                                           
         LH    R1,INPTAB                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INPTAB                                                        
*                                                                               
CHKKEY20 MVC   KEY,LASTKEY         RESET READ SEQUENCE                          
         GOTO1 HIGH                                                             
*                                                                               
         LH    R1,INPTAB                                                        
         CH    R1,=H'26'          SET FOR 2 COLS 13 ROWS                        
         BNE   RSEQ                                                             
*                                                                               
         MVC   PREVKEY,KEY                                                      
         OC    SVDEMKEY,SVDEMKEY   ARE WE IN THE MIDDLE DEMO OVERRIDES          
         BNZ   REND                                                             
         MVI   PREVKEY+12,X'FF'    SET KEY FOR RETURN                           
         B     REND                                                             
*                                                                               
NOMORE   XC    PREVKEY,PREVKEY                                                  
         XC    0(SVNPGMX-SVNPGMD,R7),0(R7)   CLEAR UP LAST UNUSED ENTRY         
         B     REND                                                             
         EJECT                                                                  
*=======================================================*                       
* DATA TABLE NOW BUILT IN REC2 - FORMAT TO SCREEN       *                       
*=======================================================*                       
         SPACE 1                                                                
REND     CLI   REC2,0              ANY DATA                                     
         BNE   FORMAT                                                           
         SPACE 2                                                                
NOTONF   MVI   ERRCD,NOFNDERR       NO - SEND MESSAGE                           
         GOTO1 ERROR                                                            
         B     MODEXIT                                                          
         SPACE 2                                                                
FORMAT   DS    0H                                                               
         XC    DMWORK(20),DMWORK                                                
         LA    R7,REC2                                                          
         SR    R8,R8                                                            
*                                                                               
FORMAT1  CLI   0(R7),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
         LA    R7,SVNPGMX-SVNPGMD(R7)                                           
         LA    R8,1(R8)                                                         
         B     FORMAT1                                                          
         SPACE 1                                                                
*===========================================================*                   
* USER2 PARMS -- SAVE DATA LEN    TABLE ADDRESS             *                   
*                                 NUMBER OF ENTRIES         *                   
*                                 NUMBER OF ROWS            *                   
*                NUMBER OF COLS   OUTPUT TABLE ADDRESS      *                   
*===========================================================*                   
         SPACE 1                                                                
FORMAT2  DS    0H                                                               
         LA    R0,SVNPGMX-SVNPGMD  SET LENGTH OF SAVE ENTRY                     
         GOTO1 USER2,DMCB,((R0),REC2),(R8),13,(2,DMWORK)                        
*                                                                               
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,FLDDATA                                                       
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
*                                                                               
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         USING SVNPGMD,R7                                                       
*                                                                               
         USING FLDDATAD,RF                                                      
         MVC   FLDNTWK,SVNTWK                                                   
         MVC   FLDCLT,SVOCLT                                                    
         MVC   FLDPROG,SVPROG                                                   
         MVC   FLDNAME,SVNAME                                                   
         MVC   FLDRTG,SVRTG                                                     
         MVC   FLDSEQ,SVSEQ                                                     
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         STC   RE,0(R6)                                                         
         L     R5,0(R6)                                                         
         LA    R5,SVNPGMX-SVNPGMD(R5)  NEXT SAVE DATA                           
         STCM  R5,7,1(R6)                                                       
         LA    R6,4(R6)                                                         
         LA    RF,FLDDATAL(RF)     POINT TO NEXT COLUMN                         
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*====================================================*                          
* READ OVERRIDE RECORDS                              *                          
*      R7 POINTS TO CURRENT ENTRY IN TABLE           *                          
*====================================================*                          
         SPACE 1                                                                
RDOVR    NTR1                                                                   
         USING SVNPGMD,R7                                                       
         MVI   HASOVRD,C'N'        DEFAULT - NO OVERRIDE FOUND                  
         MVC   THISPROG,SVPROG     1ST TIME IN                                  
         MVC   THISNAME,SVNAME                                                  
         MVC   THISNET,SVNTWK                                                   
         XC    LASTNET,LASTNET                                                  
         OC    SVDEMKEY,SVDEMKEY   LEFT OVER FROM PREV PAGE                     
         BZ    RV10                                                             
         MVC   KEY,SVDEMKEY                                                     
         B     RV20                                                             
*                                                                               
RV10     BAS   RE,GETNWK                                                        
         BNE   RVNO                                                             
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING DOVRECD,R5                                                       
         MVC   DOVKTYP,=X'0D17'                                                 
         MVC   DOVKAGMD,SVAGYMD    AGY/MED                                      
         MVC   DOVKNSEQ,NETSEQ     NETWORK SEQUENCE NUMBER                      
         MVC   DOVKPGM,THISPROG    PROGRAM                                      
*                                                                               
RV20     GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   RVNO                                                             
         B     RV40                                                             
*                                                                               
RV30     GOTO1  SEQ                                                             
*                                                                               
RV40     CLC   KEY(4),KEYSAVE                                                   
         BNE   RV80                                                             
         CLC   DOVKPGM,THISPROG    SAME PROGRAM                                 
         BNE   RV30                KEEP LOOKING                                 
         MVI   HASOVRD,C'Y'        SET - OVERRIDE FOUND                         
         MVC   SVDEMKEY,KEY                                                     
         MVI   SVDEMKEY+12,X'FF'   SET KEY FOR RETURN                           
         OC    DOVKCLT,DOVKCLT     IS THERE A CLIENT                            
         BZ    RV50                                                             
         GOTO1 =V(CLUNPK),DMCB,DOVKCLT,SVOCLT,RR=RELO                           
*                                                                               
RV50     MVC   SVRTG,=C'BBM'       RATING SERVICE                               
         CLI   DOVKRTS,C'1'                                                     
         BE    *+10                                                             
         MVC   SVRTG,=C'NSI'                                                    
         CLI   DOVKSEQ,0                                                        
         BE    RV60                                                             
         MVI   SVSEQ,C'1'                                                       
*                                                                               
RV60     GOTO1 SEQ                                                              
         MVI   SEQPROC,C'N'        SEQ RECORD READ WASN'T PUT TO TABLE          
         CLC   KEY(12),SVDEMKEY                                                 
         BNE   RV70                                                             
         MVC   SVSEQ,=C'0,1'                                                    
         MVI   SEQPROC,C'Y'                                                     
         MVC   SVDEMKEY,KEY                                                     
         MVI   SVDEMKEY+12,X'FF'   SET KEY FOR RETURN                           
*                                                                               
RV70     LA    R7,SVNPGMX-SVNPGMD(R7)                                           
         LH    R1,INPTAB                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INPTAB                                                        
         CH    R1,=H'26'                                                        
         BE    RVYES                                                            
         MVC   SVPROG,THISPROG     SET INFO IN TABLE                            
         MVC   SVNAME,THISNAME                                                  
         MVC   SVNTWK,THISNET                                                   
         CLI   SEQPROC,C'Y'                                                     
         BE    RV30                                                             
         B     RV40                                                             
*                                                                               
RV80     XC    SVDEMKEY,SVDEMKEY   CLEAR LAST DEMO KEY READ                     
         CLI   HASOVRD,C'Y'        WAS - OVERRIDE FOUND                         
         BE    RVYES                                                            
         LA    R7,SVNPGMX-SVNPGMD(R7)                                           
         LH    R1,INPTAB                                                        
         LA    R1,1(R1)                                                         
         STH   R1,INPTAB                                                        
*                                                                               
RVYES    SR    RC,RC                                                            
RVNO     LTR   RC,RC                                                            
         XIT1  REGS=(R7)                                                        
         EJECT                                                                  
*                                                                               
*        GET NETWORK RECORD TO GET NETWORK SEQUENCE NUMBER                      
*                                                                               
GETNWK   NTR1                                                                   
         CLC   LASTNET,THISNET     SAME NETWORK                                 
         BE    GNYES                                                            
         MVC   LASTNET,THISNET                                                  
         MVI   NETSEQ,0                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),THISNET                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GNNO                                                             
         GOTO1 GETREC                                                           
         L     R8,AREC                                                          
         USING NDEFRECD,R8                                                      
         LA    R6,NDEFEL                                                        
         MVI   ELCODE,X'02'        FIND NETWORK SEQUENCE NUMBER                 
         USING NDEFEL02,R6                                                      
         BAS   RE,NEXTEL                                                        
         BNE   GNNO                                                             
         MVC   NETSEQ,NDEFNET                                                   
*                                                                               
GNYES    SR    RC,RC                                                            
GNNO     LTR   RC,RC                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
RELO     DC    A(0)                                                             
DASH     DC    40C'-'                                                           
LINLEN   EQU   88                                                               
         LTORG                                                                  
         EJECT                                                                  
INPTAB   DS    H                   NUMBER OF LINES INPUT IN TABLE               
HASOVRD  DS    C                   OVERRIDE FOUND                               
SEQPROC  DS    C                   2ND SEQUENCE RECORD PROCESSED                
THISNET  DS    CL4                 CURRENT VALUES IN TABLE                      
THISPROG DS    CL4                                                              
THISNAME DS    CL12                                                             
LASTKEY  DS    CL13                LAST SHOW RECORD READ                        
*                                                                               
WRK42    DSECT                                                                  
         DS    CL4                                                              
         SPACE 2                                                                
FLDDATAD DSECT                                                                  
*                                                                               
FLDNTWK  DS    CL4                                                              
         DS    CL1                                                              
FLDCLT   DS    CL3                                                              
         DS    CL1                                                              
FLDPROG  DS    CL4                                                              
         DS    CL1                                                              
FLDNAME  DS    CL11                                                             
         DS    CL2                                                              
FLDRTG   DS    CL3                                                              
         DS    CL2                                                              
FLDSEQ   DS    CL3                                                              
         DS    CL3                                                              
FLDDATAL EQU   *-FLDDATAD                                                       
         SPACE 2                                                                
SVNPGMD  DSECT                                                                  
SVNTWK   DS    CL4                                                              
SVOCLT   DS    CL3                                                              
SVPROG   DS    CL4                                                              
SVNAME   DS    CL11                                                             
SVRTG    DS    CL3                                                              
SVSEQ    DS    CL3                                                              
SVNPGMX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENNPGM                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNDOV                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         ORG   T21AFFD+2350                                                     
         DS    0F                                                               
NETSEQ   DS    C                   NETWORK SEQUENCE BYTE                        
LASTNET  DS    CL4                 LAST NETWORK READ                            
SVDEMKEY DS    CL13                LAST DEMO REC READ                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPINF42   05/01/02'                                      
         END                                                                    
