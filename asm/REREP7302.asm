*          DATA SET REREP7302  AT LEVEL 089 AS OF 05/14/15                      
*PHASE RE7302A,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREP7302 - RE7302 - SHORT STATION LISTING'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP7302 --- SHORT STATION LISTING                        *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 26MAY95 (SKU) --- ORIGINATION                                     *           
*                                                                   *           
* 10JAN97 (BU ) --- EXPAND TO ALLOW USE BY KATZ RADIO AND TV        *           
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                                                                   *           
*                   ***  END TOMBSTONE  ***                         *           
*********************************************************************           
RE7302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7302,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REPFRST                                                     
         BE    PROC                                                             
         CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         GOTO1 REPORT              PRINT LAST ENTRY                             
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
REQF     DS    0H                                                               
         XC    PROCFLAG,PROCFLAG                                                
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
PROC     DS    0H                                                               
         TM    PROCFLAG,PFDONEQ                                                 
         BNZ   EXIT                                                             
                                                                                
         MVC   PAGE,=X'0001'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
                                                                                
         XC    SVSTA,SVSTA                                                      
         NI    PROCFLAG,X'FF'-PFMATCHQ                                          
         BAS   RE,GETIREPS                                                      
*                                                                               
         OC    IREPLIST,IREPLIST   ANY SUBSIDIARIES IN REP LIST?                
         BZ    PROC05              NO  - NOT A MASTER                           
         OI    PROCFLAG,PFISIREP   YES - SET MASTER FLAG                        
PROC05   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RST2KEY,R6                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
                                                                                
PROC10   DS    0H                                                               
         CLI   RST2KTYP,X'82'                                                   
         BNE   PROCX                                                            
                                                                                
         TM    PROCFLAG,PFISIREP   IF INTEREP                                   
         BZ    PROC15                                                           
                                                                                
         LA    R2,IREPLIST         CHECK LIST OF SUBSIDIARIES                   
         LA    R3,LREPLIST                                                      
*                                                                               
PROC13   DS    0H                                                               
         CLC   RST2KREP,0(R2)                                                   
         BE    PROC18                                                           
         LA    R2,2(R2)                                                         
         BCT   R3,PROC13                                                        
                                                                                
PROC15   DS    0H                                                               
         CLC   RST2KREP,QREP                                                    
         BNE   PROC20                                                           
                                                                                
PROC18   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         BAS   RE,FILTER                                                        
         BNZ   PROC20                                                           
         BAS   RE,PRINTSTA                                                      
                                                                                
PROC20   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEYSAVE,KEY,0                         
         B     PROC10                                                           
*                                                                               
PROCX    DS    0H                                                               
         OI    PROCFLAG,PFDONEQ                                                 
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* GET LIST OF SUBSIDIARY INTEREPS                                               
**********************************************************************          
GETIREPS NTR1                                                                   
         XC    IREPLIST,IREPLIST                                                
         XC    KEY,KEY                                                          
KEYD     USING RREPKEY,KEY                                                      
         MVI   KEYD.RREPKTYP,X'01'                                              
         MVC   KEYD.RREPKREP,RCREPFL                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
         CLC   KEYD.RREPKEY,KEYSAVE                                             
         BNE   GETIRX                                                           
         DROP  KEYD                                                             
                                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         LA    R6,IOAREA                                                        
         USING RREPSUB,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETIRX                                                           
         ZIC   R1,RREPSCNT                                                      
         SLL   R1,1                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IREPLIST(0),RREPSCOD                                             
         DROP  R6                                                               
                                                                                
GETIRX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET REP NAME                                                                  
**********************************************************************          
REPNAME  NTR1                                                                   
         XC    KEY,KEY                                                          
KEYD     USING RREPKEY,KEY                                                      
         MVI   KEYD.RREPKTYP,X'01'                                              
         MVC   KEYD.RREPKREP,P+LSTREP-LISTSTAD                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
         CLC   KEYD.RREPKEY,KEYSAVE                                             
         BNE   REPNAMEX                                                         
         DROP  KEYD                                                             
                                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
IOD      USING RREPREC,IOAREA                                                   
         MVC   P+LSTREPN-LISTSTAD(L'LSTREPN),IOD.RREPNAME                       
         DROP  IOD                                                              
                                                                                
REPNAMEX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET OWNER NAME                                                                
**********************************************************************          
OWNNAME  NTR1                                                                   
         XC    KEY,KEY                                                          
KEYD     USING ROWNKEY,KEY                                                      
         MVI   KEYD.ROWNKTYP,X'2A'                                              
         MVC   KEYD.ROWNKREP,P+LSTREP-LISTSTAD                                  
         MVC   KEYD.ROWNKOWN,P+LSTOWNER-LISTSTAD                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
         CLC   KEYD.ROWNKEY,KEYSAVE                                             
         BNE   OWNNAMEX                                                         
         DROP  KEYD                                                             
                                                                                
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         LA    R6,IOAREA                                                        
         USING ROWNREC,R6                                                       
         MVC   P+LSTOWNAM-LISTSTAD(L'LSTOWNAM),ROWNNAME                         
         DROP  R6                                                               
                                                                                
OWNNAMEX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* FILTER STATION RECORD                                                         
**********************************************************************          
FILTER   NTR1                                                                   
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
*                                                                               
         L     R4,VXRQCARD         POINT TO THE CARDS AREA                      
         LA    R4,80(R4)           POINT TO 2ND CARD                            
         USING QREC2,R4                                                         
*                                                                               
         CLI   0(R2),2             NEED 2 CARDS.                                
         BE    FILT0010                                                         
         MVC   QREC2(80),SPACES    ONLY ONE: CLEAR 2ND CARD                     
FILT0010 EQU   *                                                                
*                                                                               
         CLC   Q2OWNER,SPACES      FILTER ON OWNER?                             
         BE    FILTER10                                                         
         CLC   RSTAOWN,Q2OWNER                                                  
         BNE   NO                                                               
                                                                                
FILTER10 DS    0H                                                               
         CLC   Q2AFFIL,SPACES      FILTER ON AFFILIATE?                         
         BE    FILTER20                                                         
         CLC   RSTAAFFL,Q2AFFIL                                                 
         BNE   NO                                                               
                                                                                
FILTER20 DS    0H                                                               
         CLC   Q2TVB,SPACES        FILTER ON TVB                                
         BE    FILTER30                                                         
         CLC   RSTATVB,Q2TVB                                                    
         BNE   NO                                                               
                                                                                
FILTER30 DS    0H                                                               
         CLI   QOPTION1,C'A'       FILTER ON ACTIVE?                            
         BNE   YES                                                              
         TM    PROCFLAG,PFSTARTQ                                                
         BZ    FILTER40                                                         
         CLC   SVSTA,RSTAKSTA                                                   
         BE    YES                                                              
                                                                                
FILTER40 DS    0H                                                               
         OI    PROCFLAG,PFSTARTQ                                                
         OC    RSTAEND,RSTAEND                                                  
         BZ    YES                                                              
         NI    PROCFLAG,X'FF'-PFSTARTQ                                          
         MVC   KEY+RST2KEND-RST2KEY(3),=X'FFFFFF'                               
         B     NO                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT A STATION ENTRY                                                         
**********************************************************************          
PRINTSTA NTR1                                                                   
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         LA    R5,P                                                             
         USING LISTSTAD,R5                                                      
                                                                                
         CLI   QOPTION2,C'D'       DUPLICATE STATIONS ONLY?                     
         BNE   PRTSTA04                                                         
         OC    SVSTA,SVSTA         YES                                          
         BZ    PRTSTA05                                                         
         CLC   SVSTA,RSTAKSTA                                                   
         BE    PRTSTA03                                                         
         TM    PROCFLAG,PFMATCHQ                                                
         BZ    PRTSTA05                                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         NI    PROCFLAG,X'FF'-PFMATCHQ                                          
         B     PRTSTA05                                                         
                                                                                
PRTSTA03 DS    0H                                                               
         GOTO1 REPORT                                                           
         OI    PROCFLAG,PFMATCHQ                                                
         B     PRTSTA05                                                         
                                                                                
PRTSTA04 DS    0H                                                               
         CLC   SVSTA,RSTAKSTA                                                   
         BE    PRTSTA05                                                         
         GOTO1 REPORT                                                           
                                                                                
PRTSTA05 DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   SVSTA,RSTAKSTA                                                   
         MVC   LSTSTA(4),RSTAKSTA                                               
         CLI   RSTAKSTA+4,C' '                                                  
         BE    PRTSTA10                                                         
         MVI   LSTSTA+4,C'-'                                                    
         MVC   LSTSTA+5(1),RSTAKSTA+4                                           
                                                                                
PRTSTA10 DS    0H                                                               
* MARKET                                                                        
         MVC   LSTMKT,RSTAMKT                                                   
* REP                                                                           
         MVC   LSTREP,RSTAKREP                                                  
* JOIN DATE                                                                     
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(5,LSTJDATE)                            
* LEAVE DATE                                                                    
         GOTO1 DATCON,DMCB,(3,RSTAEND),(5,LSTLDATE)                             
* OWNER                                                                         
         MVC   LSTOWNER,RSTAOWN                                                 
* TVB                                                                           
         MVC   LSTTVB,RSTATVB                                                   
* AFFILIATE                                                                     
         MVC   LSTAFF,RSTAAFFL                                                  
* RANK                                                                          
         MVC   LSTRANK,RSTARANK                                                 
* INTERFACE CODE                                                                
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTSTA20                                                         
         USING RSTAXXEL,R6                                                      
         MVC   LSTINTF,RSTAOSI                                                  
                                                                                
PRTSTA20 DS    0H                                                               
         MVC   SVKEY,KEY                                                        
* EXPANDED REP NAME                                                             
         BAS   RE,REPNAME                                                       
* EXPANDED OWNER NAME                                                           
         OC    LSTOWNER,LSTOWNER                                                
         BZ    *+8                                                              
         BAS   RE,OWNNAME                                                       
* RESTORE SEQUENTIAL READ                                                       
         MVC   KEY,SVKEY                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
                                                                                
         CLI   QOPTION2,C'D'                                                    
         BE    PRTSTAX                                                          
         GOTO1 REPORT                                                           
                                                                                
PRTSTAX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
* STORAGE AREAS FOR BETWEEN I/O DATA                                            
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
TEMP     DS    6F                                                               
ELCODE   DS    X                                                                
SVKEY    DS    CL(L'KEY)                                                        
IREPLIST DS    CL40                TWENTY REP CODES                             
LREPLIST EQU   (*-IREPLIST)/2          NUMBER OF ENTRIES IN TABLE               
SVSTA    DS    CL5                                                              
PROCFLAG DS    X                                                                
PFMATCHQ EQU   X'80'               MATCHING STATION FOUND                       
PFDONEQ  EQU   X'40'               ALL DONE, EXIT FOR ALL MODES                 
PFSTARTQ EQU   X'20'               FOR Q1=A, ACTIVE STATION FOUND               
PFISIREP EQU   X'10'               REP IS A SUBSIDIARY REP                      
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
LISTSTAD DSECT                                                                  
         DS    CL2                                                              
LSTCODE  DS    CL1                                                              
LSTSTA   DS    CL6                                                              
         DS    CL2                                                              
LSTMKT   DS    CL20                                                             
         DS    CL2                                                              
LSTREP   DS    CL2                                                              
         DS    CL1                                                              
LSTREPN  DS    CL20                                                             
         DS    CL2                                                              
LSTJDATE DS    CL8                                                              
         DS    CL2                                                              
LSTLDATE DS    CL8                                                              
         DS    CL2                                                              
LSTOWNER DS    CL3                                                              
         DS    CL1                                                              
LSTOWNAM DS    CL20                                                             
         DS    CL2                                                              
LSTTVB   DS    CL2                                                              
         DS    CL2                                                              
LSTAFF   DS    CL3                                                              
         DS    CL2                                                              
LSTRANK  DS    CL1                                                              
         DS    CL5                                                              
LSTINTF  DS    CL10                                                             
         EJECT                                                                  
*              FILE CONTROL AND WORKD DSECTS                                    
         PRINT OFF                                                              
       ++INCLUDE REREPRGEQA                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089REREP7302 05/14/15'                                      
         END                                                                    
