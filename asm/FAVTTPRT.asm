*          DATA SET FAVTTPRT   AT LEVEL 121 AS OF 03/23/15                      
*PHASE VTTA                                                                     
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE REGSAVE                                                                
         TITLE '$VTT- PRINT VTAM DATA FROM ADRFILE'                             
         PRINT NOGEN                                                            
VTT      CSECT                                                                  
         NBASE 0,**VTT,=V(REGSAVE)                                              
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         MVI   NOSFLG,0                                                         
         MVI   YESSFLG,0                                                        
         MVI   RPLFLG,0                                                         
         MVI   OPNFLG,0                                                         
         MVI   LUFLG,0                                                          
         MVC   MID1(67),=CL67'LUNAME    DATE      TIME      COMMAND   RX        
               CFD  SENSE     DATA'                                             
         MVC   MID2(67),=CL67'------    ----      ----      -------   -X        
               ---  -----     ----'                                             
         OPEN  (ADRFILE,INPUT)                                                  
*                                                                               
*                                                                               
*                                                                               
*TEST FOR PRINTING SPECIFICATIONS                                               
RDCARD   GOTO1 =V(CARDS),DMCB,CARDIO,=C'REOO'                                   
         CLC   EOFC,CARDIO         CHECK FOR END OF FILE                        
         BE    CDDONE                                                           
         CLC   DETAIL,CARDIO       PRINT RPL IN DETAIL?                         
         BNE   *+12                                                             
         MVI   RPLFLG,1            TURN ON RPL FLAG                             
         B     ENDLP                                                            
         CLC   SORTY,CARDIO        DO SORT?                                     
         BNE   *+12                                                             
         MVI   YESSFLG,1                                                        
         B     ENDLP                                                            
         CLC   OPNY,CARDIO                                                      
         BNE   *+12                                                             
         MVI   OPNFLG,1                                                         
         B     ENDLP                                                            
         CLC   LUIDY,CARDIO                                                     
         BNE   ENDLP                                                            
         MVC   LUSAVE(8),CARDIO+5  SAVE OFF LUID                                
         MVI   LUFLG,1                                                          
ENDLP    B     RDCARD                                                           
*                                                                               
*                                                                               
CDDONE   CLI   LUFLG,X'01'         PRINT ONLY SPECIFIC LUID'S                   
         BNE   NOLU                                                             
         MVI   YESSFLG,0           IF SO DON'T SORT                             
         MVI   OPNFLG,1            PRINT ALL OPNDST                             
         MVI   RPLFLG,1            PRINT RPL IN DETAIL                          
NOLU     CLI   YESSFLG,X'01'                                                    
         BNE   NOSORT                                                           
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     RDLOOP                                                           
NOSORT   GOTO1 =V(SORTER),DMCB,STCARD,RECCARD,0                                 
RDLOOP   GET   ADRFILE,BUFF        READ FROM FILE INTO BUFF                     
         CLC   BUFF(5),=C'*VTAM'                                                
         BNE   NXTBLOCK                                                         
         L     R8,=A(BUFF)                                                      
         LA    R8,10(R8)           POINT TO FIRST 188 BYTE RECORD               
         USING VTTRCD,R8           VTTRCD DSECT COVERS 188BYTE RECORD           
         LA    R2,(6400-10)/VTTRCLEN  MAX OF 33 RECORDS !                       
*                                                                               
RECORDS  CLI   VTTRCLU,X'00'       CHECK FOR INCOMPLETE RECORD                  
         BE    NXTBLOCK                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R8)                                     
         LA    R8,VTTRCLEN(R8)                                                  
         BCT   R2,RECORDS          STILL MORE RECORDS TO PROCESS                
NXTBLOCK B     RDLOOP              GET NEXT BLOCK                               
*                                                                               
FINISH   LA    R5,P                PREPARE FOR MOVING INTO PRINT LN             
         USING PRNTD,R5                                                         
PRNTLP   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R8,15,4(R1)         GET ADDRESS OF SORTED RECORD                 
         BZ    NORECS                                                           
         CLI   LUFLG,X'01'                                                      
         BE    NOCHANGE                                                         
         CLC   VTTRCLU(8),LUTEMP                                                
         BE    NOCHANGE                                                         
         MVC   LUTEMP,VTTRCLU                                                   
         MVI   P,C'-'                                                           
         MVC   P+1(131),P                                                       
         GOTO1 =V(PRINTER)                                                      
NOCHANGE MVC   PLUNAME,VTTRCLU     MOVE IN LU NAME                              
         CLI   LUFLG,X'01'                                                      
         BNE   *+14                                                             
         CLC   LUSAVE,PLUNAME                                                   
         BNE   NODETAIL                                                         
         GOTO1 =V(DATCON),DMCB,(6,VTTRCDAT),(8,PDATE)                           
*                                                                               
* MOVE IN DATE                                                                  
         ICM   R0,15,VTTRCTIM      PICK UP HHMMSSTF                             
         SRL   R0,4                DROP HUNDREDTHS                              
         XC    DUB,DUB                                                          
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'         MAKE TENTHS INTO SIGN BITS                   
         MVC   WORK(10),=X'402120204B20204B2020'                                
         ED    WORK(10),DUB+4                                                   
         MVC   PTIME,WORK+2                                                     
*                                                                               
         LA    R7,VTTRCRPL        POINT TO RPL                                  
         USING IFGRPL,R7                                                        
*                                                                               
* MOVE IN COMMAND                                                               
         ZIC   RF,RPLREQ           GET OP CODE                                  
         N     RF,=X'0000007F'                                                  
         SLL   RF,3                X 8                                          
         LA    RE,VTAMCMDS                                                      
         AR    RE,RF                                                            
         MVC   PCOMMAND,0(RE)                                                   
         TM    RPLREQ,X'80'                                                     
         BZ    *+16                                                             
         MVC   PCOMMAND(2),=C'DD'                                               
         MVC   PCOMMAND+2(6),0(RE)                                              
*                                                                               
* FILL IN RCFD                                                                  
         GOTO1 =V(HEXOUT),DMCB,RPLRTNCD,PRCFD,2,=C'TOG'                         
         ICM   R3,15,12(R1)        GET 4TH PARAMETER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FILL IN SENSE                                                                 
         GOTO1 =V(HEXOUT),DMCB,RPLSSEI,PSENSE,4,=C'TOG'                         
         ICM   R3,15,12(R1)        GET 4TH PARAMETER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
* FILL IN DATA                                                                  
         MVC   PDATA,VTTRCDTA                                                   
*                                                                               
* CHECK FOR OPNDST REQUEST                                                      
         CLI   OPNFLG,X'01'        IF YES PRINT OPNDST RECORDS                  
         BE    PRINTIT                                                          
         CLC   PCOMMAND(6),=C'OPNDST'                                           
         BNE   PRINTIT                                                          
         CLC   PRCFD(4),=C'0000'                                                
         BE    NODETAIL                                                         
PRINTIT  GOTO1 =V(PRINTER)                                                      
*                                                                               
* CONVERT DATA TO HEX                                                           
         GOTO1 =V(HEXOUT),DMCB,VTTRCDTA,AREA,64,=C'SEP'                         
         ICM   R3,15,12(R1)        GET 4TH PARAMETER                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PDATA,AREA                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   PDATA,AREA+64                                                    
         GOTO1 =V(PRINTER)                                                      
         MVI   P,C' '                                                           
         GOTO1 =V(PRINTER)                                                      
*FILL IN RPL DATA FIELD                                                         
         CLI   RPLFLG,X'01'                                                     
         BNE   NODETAIL                                                         
         GOTO1 =V(HEXOUT),DMCB,VTTRCRPL,PRPL,108,=C'SEP'                        
         MVC   P(108),PRPL                                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(108),PRPL+108                                                  
         GOTO1 =V(PRINTER)                                                      
         MVI   P,C' '                                                           
         GOTO1 =V(PRINTER)                                                      
NODETAIL B     PRNTLP                                                           
*                                                                               
NORECS   CLOSE (ADRFILE)                                                        
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
ADRFILE  DCB   DDNAME=ADRFILE,DSORG=PS,RECFM=F,MACRF=GM,               X        
               EODAD=FINISH                                                     
SORTCARD DC    CL80'SORT FIELDS=(1,8,A,13,4,A),FORMAT=BI'                       
STCARD   DC    CL80'SORT FIELDS=(13,4,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=188'                                   
AREA     DS    CL128                                                            
PRPL     DS    CL216                                                            
CARDIO   DS    CL80                                                             
LUSAVE   DS    CL8                                                              
LUTEMP   DC    CL8'        '                                                    
WORK     DS    CL17                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
EOFC     DC    CL2'/*'                                                          
DETAIL   DC    CL7'RPL=YES'                                                     
SORTY    DC    CL9'SORT=LUID'                                                   
SORTN    DC    CL9'SORT=TIME'                                                   
OPNY     DC    CL10'OPNDST=YES'                                                 
LUIDY    DC    CL5'LUID='                                                       
NOSFLG   DC    X'0'                                                             
YESSFLG  DC    X'0'                                                             
RPLFLG   DC    X'0'                                                             
OPNFLG   DC    X'0'                                                             
LUFLG    DC    X'0'                                                             
*                                                                               
         EJECT                                                                  
VTAMCMDS DS    0D                                                               
         DC    CL8'????????'       00                                           
         DC    CL8'????????'       01                                           
         DC    CL8'CHECK   '       02                                           
         DC    CL8'????????'       03                                           
         DC    CL8'ENDREQ  '       04                                           
         DC    CL8'????????'       05                                           
         DC    CL8'VERIFY  '       06                                           
         DC    CL8'IMPORT  '       07                                           
         DC    CL8'DTAPRFMT'       08                                           
         DC    CL8'IX PRFMT'       09                                           
         DC    CL8'FORCE IO'       0A                                           
         DC    CL8'????????'       0B                                           
         DC    CL8'????????'       0C                                           
         DC    CL8'????????'       0D                                           
         DC    CL8'????????'       0E                                           
         DC    CL8'????????'       0F                                           
         DC    CL8'????????'       10                                           
         DC    CL8'WRITE   '       11                                           
         DC    CL8'RESET   '       12                                           
         DC    CL8'DO      '       13                                           
         DC    CL8'VER REFR'       14                                           
         DC    CL8'SETLOGON'       15                                           
         DC    CL8'SIMLOGON'       16                                           
         DC    CL8'OPNDST  '       17                                           
         DC    CL8'????????'       18                                           
         DC    CL8'CHANGE  '       19                                           
         DC    CL8'INQUIRE '       1A                                           
         DC    CL8'INTRPRT '       1B                                           
         DC    CL8'????????'       1C                                           
         DC    CL8'READ    '       1D                                           
         DC    CL8'SOLICIT '       1E                                           
         DC    CL8'CLSDST  '       1F                                           
         DC    CL8'????????'       20                                           
         DC    CL8'CLOSEACB'       21                                           
         DC    CL8'SEND    '       22                                           
         DC    CL8'RECEIVE '       23                                           
         DC    CL8'RESETSR '       24                                           
         DC    CL8'SESSIONC'       25                                           
         DC    CL8'????????'       26                                           
         DC    CL8'SENDCMD '       27                                           
         DC    CL8'RCVCMD  '       28                                           
         DC    CL8'REQSESS '       29                                           
         DC    CL8'OPNSEC  '       2A                                           
         DC    CL8'CLSSEC  '       2B                                           
         DC    CL8'TRMSESS '       2C                                           
         DC    CL8'????????'       2D                                           
         DC    CL8'????????'       2E                                           
         DC    CL8'????????'       2F                                           
         EJECT                                                                  
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(VTT,65000)                                                     
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
BUFF     DS    6400C                                                            
*                                                                               
*                                                                               
*                                                                               
BUFFD    DSECT                                                                  
BVTAM    DS    CL10                                                             
*                                                                               
PRNTD    DSECT                                                                  
PLUNAME  DS    CL8                                                              
         DS    CL2                                                              
PDATE    DS    CL8                                                              
         DS    CL2                                                              
PTIME    DS    CL8                                                              
         DS    CL2                                                              
PCOMMAND DS    CL8                                                              
         DS    CL2                                                              
PRCFD    DS    CL4                                                              
         DS    CL2                                                              
PSENSE   DS    CL8                                                              
         DS    CL2                                                              
PDATA    DS    CL64                                                             
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE VTTRCBUFF                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
         IFGRPL AM=VTAM                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121FAVTTPRT  03/23/15'                                      
         END                                                                    
