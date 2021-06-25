*          DATA SET ACLDXTIMX  AT LEVEL 006 AS OF 07/22/09                      
*PHASE ACXACXA                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
***********************************************************************         
* SEE RAJIV GUPTA BEFORE DELETING OR USING THIS BOOK                  *         
* THIS JOB CONVERTS TIME RECORDS FOR BRAND OCEAN                      *         
* TO RUN THIS YOU NEED                                                *         
* LOAD=ACXACX                                                         *         
* PARAM=HEX COMPANY CODE FOLLOWED BY FF'   EX PARAM=DBFF              *         
* CARDS IN AC101 JCL TO RUN THIS.                                     *         
***********************************************************************         
DMLDEXT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         B     DMXCTL                                                           
ORG      DC    C'*VERIFY*050129051230',C'X',X'FF'                               
         ORG   ORG+8                                                            
         DC    X'FA'                                                            
         ORG   *+5                                                              
         DC    X'FA'                                                            
         ORG                                                                    
COMPANY  DS    X                                                                
DMXRTST  DS    0H                                                               
*        B     DMXPURGE            PURGE WHEN TESTING                           
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
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
                                                                                
DMXINIT  DS    0H                                                               
         J     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
DMXRET   DS    0H                                                               
         J     DMXIT                                                            
***********************************************************************         
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
DMXEOF   B     DMXIT                                                            
***********************************************************************         
         SPACE                                                                  
         EJECT                                                                  
DMXREC   MVI   RECFLAG,0                                                        
         MVI   SORTSW,0                                                         
         L     R2,VREC                                                          
         USING TIMRECD,R2                                                       
         GOTO1 VRECTYP,DMCB,(C'D',TIMRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         MVC   COMPANY,1(R1)                                                    
*                                                                               
*        L     R4,APARAMC         PARAM=COMPANY CODE FOLLOWED BY X'FF'          
*DMXR10   GOTOR CHEXIN,DMCB,(R4),CPYCODE,2,0                                    
*        CLI   CPYCODE,X'40'          NO COMPANY CODE PASSED                    
*        BNH   DMXRTST                                                          
*        CLI   CPYCODE,X'FF'            IS IT X'FF'                             
*        BE    DMXRTST                                                          
*        CLC   COMPANY,CPYCODE                                                  
*        BE    DMXR20                                                           
*        LA    R4,2(R4)               POINT TO NEXT COMPANY CODE.               
*        B     DMXR10                                                           
*                                                                               
DMXR20   DS    0H                                                               
         CLI   RECTYPE,ACRTTIM                                                  
         BNE   DMXRTST                                                          
         CLI   TIMRRI1,X'61'                                                    
         BNE   DMXRTST                                                          
*                                                                               
DMXR30   MVC   P+4(14),TIMKUNT                                                  
         XOUT  TIMRRI1,P+20,1                                                   
         MVI   TIMRRI1,X'40'                                                    
*                                                                               
         L     R2,VISREC                                                        
         XOUT  TIMKRI1,P+24,1                                                   
         MVI   TIMKRI1,X'40'                                                    
*                                                                               
*        GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
VRECTYP  DC    V(ACRECTYP)                                                      
VBINSRCH DC    V(BINSRCH)                                                       
VSORTER  DC    V(SORTER)                                                        
VHELLO   DC    V(HELLO)                                                         
CUREDIT  DC    V(CUREDIT)                                                       
CHEXIN   DC    V(HEXIN)                                                         
ACCMST   DC    C'ACCMST  '                                                      
DUPTABN  DC    F'0'                                                             
                                                                                
CONVERTS DC    PL6'0'                                                           
DELETES  DC    PL6'0'                                                           
                                                                                
SORTSRT  DC    C'SORT FIELDS=(5,42,A),FORMAT=BI,WORK=1 '                        
SORTRCD  DC    C'RECORD TYPE=V,LENGTH=(2000,,,,) '                              
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
SORTSW   DC    X'00'                                                            
                                                                                
LASTKEY  DC    XL(L'ACTKEY)'00'                                                 
*                                                                               
TOTAL    DC    PL8'0'                                                           
TOTALD   DC    PL8'0'                                                           
TOTALC   DC    PL8'0'                                                           
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
APEELSA  DS    A                                                                
COUNT    DS    F                                                                
COUNT2   DS    F                                                                
WORK     DS    XL64                                                             
*                                                                               
         DS    0D                                                               
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
SORTIT   DS    C                                                                
WIPEIT   DS    C                                                                
RECTYPE  DS    X                                                                
RECFLAG  DS    X                                                                
ELEMENT  DS    XL256                                                            
CPYCODE  DS    X                                                                
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
**PAN#1  DC    CL21'006ACLDXTIMX 07/22/09'                                      
         END                                                                    
